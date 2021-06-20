{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, RecordWildCards
  #-}

module Bot.Telegram
    ( module Bot
    , doBotThing
    , withHandle
    , mergeStrings
    , Config(..)
    ) where

import qualified API
import qualified API.Telegram as TG
import API.Telegram.Types
import Bot hiding (strings)
import qualified Bot (strings)
import Control.Exception (finally)
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
import Data.List.Extended (changeSubseq)
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

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        }
    deriving (Show)

mergeStrings :: Config -> Bot.Strings -> Config
mergeStrings cfg ss = cfg {strings = strings cfg <> ss}

new :: Config -> IO (Handle IO)
new cfg@Config {..} = do
    state <- newIORef $ BotState {userSettings = mempty}
    hAPI <- TG.new TG.Config {..}
    return $ Handle {..}

withHandle :: Config -> (Handle IO -> IO a) -> IO a
withHandle config io = do
    hBot <- new config
    io hBot

doBotThing :: Handle IO -> IO [L8.ByteString]
doBotThing hBot = do
    updates <- fetchUpdates hBot
    requests <- reactToUpdates hBot updates
    mapM (hBot & hAPI & API.sendRequest) requests

fetchUpdates :: Handle IO -> IO [Update]
fetchUpdates hBot = do
    req <- hBot & hAPI & TG.getUpdates
    json <- hBot & hAPI & API.sendRequest $ req
    resp <- throwDecode json
    extractUpdates resp

getUserMultiplier :: Handle m -> User -> IO Int
getUserMultiplier hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = Bot.echoMultiplier hBot
        uhash = hashUser user
        repeats = Map.findWithDefault drepeats uhash $ userSettings st
    return repeats

getUserMultiplierM :: Handle m -> Maybe User -> IO Int
getUserMultiplierM hBot (Just u) = hBot & getUserMultiplier $ u
getUserMultiplierM hBot Nothing = return $ hBot & Bot.echoMultiplier

setUserMultiplier :: Handle m -> User -> Int -> IO ()
setUserMultiplier hBot user repeats = do
    hBot `Bot.hSetState` \st ->
        let uhash = hashUser user
            usettings = Map.alter (const $ Just repeats) uhash $ userSettings st
         in st {userSettings = usettings}

reactToUpdates :: Handle IO -> [Update] -> IO [API.Request]
reactToUpdates hBot updates = do
    requests <- join <$> mapM (reactToUpdate hBot) updates
    return requests `finally` remember updates
  where
    remember [] = return ()
    remember us = TG.rememberLastUpdate (hAPI hBot) $ last us

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

reactToUpdate :: Handle IO -> Update -> IO [API.Request]
reactToUpdate hBot update = do
    let qu = qualifyUpdate update
    case qu of
        ECommand msg -> (: []) <$> reactToCommand hBot msg
        EMessage msg -> reactToMessage hBot msg
        ECallback cq -> (: []) <$> reactToCallback hBot cq
        EOther Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id

reactToCommand :: Handle IO -> Message -> IO API.Request
reactToCommand hBot msg = do
    cmd <- getCommandThrow msg
    action <- getActionThrow cmd
    runAction action hBot msg

reactToMessage :: Handle IO -> Message -> IO [API.Request]
reactToMessage hBot msg = do
    author <- getAuthorThrow msg
    n <- hBot `getUserMultiplier` author
    n `replicateM` TG.copyMessage msg

data QueryData
    = QDRepeat Int
    | QDOther String
    deriving (Show)

qualifyQuery :: String -> QueryData
qualifyQuery qstring =
    case qtype of
        "repeat" -> QDRepeat $ read (tail qdata)
        _ -> QDOther qstring
  where
    (qtype, qdata) = break (== '_') qstring

reactToCallback :: Handle IO -> CallbackQuery -> IO API.Request
reactToCallback hBot cq@CallbackQuery {id, from} = do
    cdata <- getQDataThrow cq
    let user = from
    case qualifyQuery cdata of
        QDRepeat n -> do
            setUserMultiplier hBot user n
            TG.answerCallbackQuery (hAPI hBot) id
        QDOther s ->
            throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

newtype Action m =
    Action
        { runAction :: Handle m -> Message -> m API.Request
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: (Monad m) => Map.Map BotCommand (Action m)
commands =
    Map.fromList
        [ ( BotCommand {command = "start", description = "Greet User"}
          , Action
                (\Handle {strings} Message {chat} ->
                     TG.sendMessage ((chat :: Chat) & chat_id) $
                     Bot.greeting strings))
        , ( BotCommand {command = "help", description = "Show help text"}
          , Action
                (\Handle {strings} Message {chat} ->
                     TG.sendMessage ((chat :: Chat) & chat_id) $
                     Bot.help strings))
        , ( BotCommand
                { command = "repeat"
                , description = "Set number of message repeats to make"
                }
          , Action
                (\Handle {strings} Message {chat} ->
                     TG.sendInlineKeyboard
                         ((chat :: Chat) & chat_id)
                         (Bot.repeat strings)
                         repeatKeyboard))
        ]

repeatPrompt :: Handle m -> Maybe User -> IO String
repeatPrompt hBot userM = do
    mult <- hBot & getUserMultiplierM $ userM
    let prompt' = hBot & Bot.strings & Bot.repeat
    return $ changeSubseq prompt' "%n" (show mult)

getActionThrow :: (MonadThrow m) => String -> m (Action IO)
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

isCommandE :: Message -> Bool
isCommandE Message {text} =
    case text of
        Just t -> isCommand t && isKnownCommand t
        Nothing -> False
