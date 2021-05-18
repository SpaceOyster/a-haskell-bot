{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Bot.Telegram where

import qualified API
import qualified API.Telegram as TG
import API.Telegram.Types
import Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadCatch, MonadThrow(..), handleAll)
import Data.Function ((&))
import Data.IORef (IORef)
import qualified Data.Map as Map
    ( Map
    , alter
    , findWithDefault
    , fromList
    , keys
    , lookup
    , mapKeys
    )
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))

data BotState =
    BotState
        { lastUpdate :: Integer
        , userSettings :: Map.Map User Int
        }

getUserSettings :: Handle m BotState -> User -> IO Int
getUserSettings hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = defaultRepeat hBot
        repeats = Map.findWithDefault drepeats user $ userSettings st
    return repeats

setUserSettings :: Handle m BotState -> User -> Int -> IO ()
setUserSettings hBot user repeats = do
    hBot `Bot.hSetState` \st ->
        let usettings = Map.alter (const $ Just repeats) user $ userSettings st
         in st {userSettings = usettings}

reactToUpdate :: Handle IO BotState -> Update -> IO [API.Request]
reactToUpdate hBot update = do
    let qu = TG.qualifyUpdate update
    case qu of
        TG.ECommand msg -> (: []) <$> reactToCommand hBot msg
        TG.EMessage msg -> reactToMessage hBot msg
        TG.ECallback cq -> (: []) <$> reactToCallback hBot cq
        TG.EOther Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id

reactToCommand :: Handle IO state -> Message -> IO API.Request
reactToCommand hBot msg = do
    cmd <- getCommandThrow msg
    action <- TG.getActionThrow cmd
    TG.runAction action (api hBot) msg

reactToMessage :: Handle IO BotState -> Message -> IO [API.Request]
reactToMessage hBot msg = do
    author <- getAuthorThrow msg
    n <- hBot `getUserSettings` author
    n `replicateM` TG.copyMessage msg

reactToCallback :: Handle IO BotState -> CallbackQuery -> IO API.Request
reactToCallback hBot cq@CallbackQuery {id, from} = do
    cdata <- getQDataThrow cq
    let user = from
    case TG.qualifyQuery cdata of
        TG.QDRepeat n -> do
            setUserSettings hBot user n
            TG.answerCallbackQuery (api hBot) id
        TG.QDOther s ->
            throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

newtype Action m =
    Action
        { runAction :: Handle m BotState -> Message -> m API.Request
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: (Monad m) => Map.Map BotCommand (Action m)
commands =
    Map.fromList
        [ ( BotCommand {command = "start", description = "Greet User"}
          , Action
                (\Handle {greeting} Message {chat} ->
                     TG.sendMessage ((chat :: Chat) & chat_id) greeting))
        , ( BotCommand {command = "help", description = "Show help text"}
          , Action
                (\Handle {helpMessage} Message {chat} ->
                     TG.sendMessage ((chat :: Chat) & chat_id) helpMessage))
        , ( BotCommand
                { command = "repeat"
                , description = "Set number of message repeats to make"
                }
          , Action
                (\Handle {repeatPrompt} Message {chat} ->
                     TG.sendInlineKeyboard
                         ((chat :: Chat) & chat_id)
                         repeatPrompt))
        ]

getActionThrow :: (MonadThrow m) => String -> m (Action m)
getActionThrow cmd =
    case Map.lookup cmd $ command `Map.mapKeys` commands of
        Just a -> return a
        Nothing -> throwM $ Ex Priority.Info $ "Unknown command: " ++ cmd

commandsList :: [String]
commandsList =
    command <$> Map.keys (commands :: Map.Map BotCommand (Action Maybe))

repeatKeyboard :: InlineKeyboardMarkup
repeatKeyboard =
    InlineKeyboardMarkup [[button 1, button 2, button 3, button 4, button 5]]
  where
    button x =
        InlineKeyboardButton
            {text = show x, callback_data = "repeat_" ++ show x}
