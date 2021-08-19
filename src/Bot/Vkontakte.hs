{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Vkontakte where

import qualified API
import qualified API.Vkontakte as VK
import Bot hiding (strings)
import qualified Bot (strings)
import Control.Monad (join)
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

new :: Config -> IO (Handle VK.VKState)
new cfg@Config {..} = do
    let hLog = undefined
    state <- newIORef $ BotState {userSettings = mempty}
    hAPI <- VK.new VK.Config {..}
    pure $ Handle {..}

withHandle :: Config -> (Handle VK.VKState -> IO a) -> IO a
withHandle config io = do
    hBot <- new config
    io hBot

doBotThing :: Handle VK.VKState -> IO [L8.ByteString]
doBotThing hBot@Handle {hLog} = do
    updates <- fetchUpdates hBot
    requests <- reactToUpdates hBot updates
    Logger.info' hLog $
        "Vkontakte: sending " <> show (length requests) <> " responses"
    mapM (hBot & hAPI & API.sendRequest) requests

fetchUpdates :: Handle VK.VKState -> IO [VK.GroupEvent]
fetchUpdates hBot@Handle {hAPI} = do
    req <- hAPI & VK.getUpdates
    json <- hAPI & API.sendRequest $ req
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
        ECallback msg -> undefined
        EOther _ -> throwM $ Ex Priority.Info "Unknown Update Type."

reactToCommand :: Handle VK.VKState -> VK.Message -> IO API.Request
reactToCommand hBot msg = do
    undefined

reactToMessage :: Handle VK.VKState -> VK.Message -> IO [API.Request]
reactToMessage hBot msg = do
    undefined

data QueryData
    = QDRepeat Int
    | QDOther String
    deriving (Show)

-- diff
isCommandE :: VK.Message -> Bool
isCommandE VK.Message {text} = isCommand text
