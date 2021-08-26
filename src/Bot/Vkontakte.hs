{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Vkontakte where

import qualified API
import qualified API.Vkontakte as VK
import Bot hiding (strings)
import qualified Bot (strings)
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (newIORef)
import Exceptions (BotException(..))
import qualified Exceptions as Priority (Priority(..))
import qualified Logger
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        , group_id :: Integer
        , v :: String
        }
    deriving (Show)

new :: Config -> Logger.Handle -> IO (Handle VK.VKState)
new cfg@Config {..} hLog = do
    state <- newIORef $ BotState {userSettings = mempty}
    hAPI <- VK.new VK.Config {..} hLog
    pure $ Handle {..}

withHandle :: Config -> Logger.Handle -> (Handle VK.VKState -> IO a) -> IO a
withHandle config hLog io = do
    hBot <- new config hLog
    io hBot

doBotThing :: Handle VK.VKState -> IO [L8.ByteString]
doBotThing hBot@Handle {hLog} = do
    updates <- fetchUpdates hBot
    requests <- reactToUpdates hBot updates
    Logger.info' hLog $
        "Vkontakte: sending " <> show (length requests) <> " responses"
    mapM (hBot & hAPI & API.sendRequest) requests

fetchUpdates :: Handle VK.VKState -> IO [VK.GroupEvent]
fetchUpdates hBot@Handle {hAPI, hLog} = do
    Logger.info' hLog "Vkontakte: fetching Updates"
    req <- hAPI & VK.getUpdates
    json <- hAPI & API.sendRequest $ req
    Logger.debug' hLog $ "Vkontakte: decoding json response: " <> L8.unpack json
    resp <- throwDecode json
    VK.extractUpdates =<< VK.rememberLastUpdate hAPI resp

reactToUpdates :: Handle VK.VKState -> [VK.GroupEvent] -> IO [API.Request]
reactToUpdates hBot updates = do
    join <$> mapM (reactToUpdate hBot) updates

data Entity
    = EMessage VK.Message
    | ECommand VK.Message
    | ECallback VK.Message -- TODO
    | EOther VK.GroupEvent
    deriving (Show)

-- diff
qualifyUpdate :: VK.GroupEvent -> Entity
qualifyUpdate (VK.MessageNew m) =
    if isCommandE m
        then ECommand m
        else EMessage m
qualifyUpdate _ = EOther VK.Other -- TODO

reactToUpdate :: Handle VK.VKState -> VK.GroupEvent -> IO [API.Request]
reactToUpdate hBot update = do
    let qu = qualifyUpdate update
    case qu of
        ECommand msg -> (: []) <$> reactToCommand hBot msg
        EMessage msg -> reactToMessage hBot msg
        ECallback cq -> (: []) <$> reactToCallback hBot cq
        EOther _ -> throwM $ Ex Priority.Info "Unknown Update Type."

reactToCommand :: Handle VK.VKState -> VK.Message -> IO API.Request
reactToCommand hBot@Handle {hLog} msg@VK.Message {id, peer_id} = do
    let cmd = getCommand msg
    Logger.debug' hLog $
        "Vkontakte: Got command" <>
        show cmd <>
        " in message id " <> show id <> " , peer_id: " <> show peer_id
    let action = commandAction cmd
    runAction action hBot msg

-- diff
reactToMessage :: Handle VK.VKState -> VK.Message -> IO [API.Request]
reactToMessage hBot@Handle {hAPI, hLog} msg@VK.Message {..} = do
    n <- getUserMultiplier hBot $ VK.User from_id
    clone <- VK.copyMessage hAPI msg
    Logger.debug' hLog $
        "Vkontakte: generating " <> show n <> " echoes for Message: " <> show id
    Logger.debug' hLog $ "  Multiplied request" <> show clone
    n `replicateM` pure clone

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

-- diff
reactToCallback :: Handle VK.VKState -> VK.Message -> IO API.Request
reactToCallback hBot cq@VK.Message {id, from_id, payload} = do
    let cdata = undefined
    let user = from_id
    case qualifyQuery cdata of
        QDRepeat n -> undefined
        QDOther s ->
            throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

getCommand :: VK.Message -> Command
getCommand = parseCommand . takeWhile (/= ' ') . tail . VK.text

commandAction :: Command -> Action VK.VKState IO
commandAction cmd =
    Action $ \hBot@Handle {..} VK.Message {..} -> do
        let address = peer_id
        case cmd of
            Start -> VK.sendTextMessage hAPI address $ Bot.greeting strings
            Help -> VK.sendTextMessage hAPI address $ Bot.help strings
            Repeat -> VK.sendKeyboard hAPI address "Hello World" repeatKeyboard
            UnknownCommand ->
                VK.sendTextMessage hAPI address $ Bot.unknown strings

newtype Action s m =
    Action
        { runAction :: Handle s -> VK.Message -> m API.Request
        }

-- diff
isCommandE :: VK.Message -> Bool
isCommandE VK.Message {text} = isCommand text
