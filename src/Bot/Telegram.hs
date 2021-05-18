{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, RecordWildCards
  #-}

module Bot.Telegram where

import qualified API
import qualified API.Telegram as TG
import API.Telegram.Types
import Bot
import Control.Exception (bracket, finally)
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadCatch, MonadThrow(..), handleAll)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
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
import System.Environment (getEnv)
import Utils (throwDecode)

data BotState =
    BotState
        { lastUpdate :: Integer
        , userSettings :: Map.Map User Int
        }

data Config =
    Config
        { key :: String
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        , defaultRepeat :: Int
        }

new :: Config -> IO (Handle IO BotState)
new cfg@Config {..} = do
    state <- newIORef $ BotState {lastUpdate = 0, userSettings = mempty}
    apiCfg <- TG.parseConfig
    api <- TG.new apiCfg
    return $
        Handle {api, state, helpMessage, greeting, repeatPrompt, defaultRepeat}

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    let helpMessage = "This is a help message"
        greeting = "This is greeting message"
        repeatPrompt = "How many times you want your messages to be repeated?"
    return $
        Config {key, helpMessage, greeting, repeatPrompt, defaultRepeat = 1}

getUserSettings :: Handle m BotState -> User -> IO Int
getUserSettings hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = Bot.defaultRepeat hBot
        repeats = Map.findWithDefault drepeats user $ userSettings st
    return repeats

setUserSettings :: Handle m BotState -> User -> Int -> IO ()
setUserSettings hBot user repeats = do
    hBot `Bot.hSetState` \st ->
        let usettings = Map.alter (const $ Just repeats) user $ userSettings st
         in st {userSettings = usettings}

reactToUpdates :: Handle IO BotState -> L8.ByteString -> IO [API.Request]
reactToUpdates hBot json = do
    resp <- throwDecode json
    updates <- extractUpdates resp
    requests <- mapM (reactToUpdate hBot) updates
    return (join requests) `finally` remember updates
  where
    remember [] = return ()
    remember us = TG.rememberLastUpdate (api hBot) $ last us

reactToUpdate :: Handle IO BotState -> Update -> IO [API.Request]
reactToUpdate hBot update = do
    let qu = qualifyUpdate update
    case qu of
        ECommand msg -> (: []) <$> reactToCommand hBot msg
        EMessage msg -> reactToMessage hBot msg
        ECallback cq -> (: []) <$> reactToCallback hBot cq
        EOther Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id

reactToCommand :: Handle IO BotState -> Message -> IO API.Request
reactToCommand hBot msg = do
    cmd <- getCommandThrow msg
    action <- getActionThrow cmd
    runAction action hBot msg

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

isKnownCommand :: String -> Bool
isKnownCommand s = tail s `elem` commandsList

data Entity
    = EMessage Message
    | ECommand Message
    | ECallback CallbackQuery
    | EOther Update
    deriving (Show)

qualifyUpdate :: Update -> Entity
qualifyUpdate u@Update {message, callback_query}
    | Just cq <- callback_query = ECallback cq
    | Just msg <- message =
        if isCommandE msg
            then ECommand msg
            else EMessage msg
    | otherwise = EOther u

isCommandE :: Message -> Bool
isCommandE Message {text} =
    case text of
        Just t -> isCommand t && isKnownCommand t
        Nothing -> False
