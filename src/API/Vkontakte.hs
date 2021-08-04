{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Vkontakte
    (
    ) where

import qualified API
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Exceptions as Ex
import GHC.Generics
import qualified HTTP
import qualified Network.URI.Extended as URI
import Utils (throwDecode)

type APIState = String

type Handle = API.Handle APIState

data Config =
    Config
        { key :: String
        , group_id :: Integer
        , v :: String
        }

new :: Config -> IO Handle
new cfg@Config {..} = do
    http <- HTTP.new $ HTTP.Config {}
    pollServer <- getLongPollServer http $ cfg {v = "5.86"}
    baseURI <- makeBaseURI pollServer
    apiState <- newIORef $ ts (pollServer :: PollServer)
    pure $ API.Handle {http, hLog = undefined, baseURI, apiState}

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config io = do
    hAPI <- new config
    io hAPI

data PollServer =
    PollServer
        { key :: String
        , server :: String
        , ts :: String
        }
    deriving (Show, Generic, FromJSON)

newtype APIResponse =
    Response
        { response :: PollServer
        }
    deriving (Show, Generic, FromJSON)

data PollResponse =
    PollResponse
        { ts :: String
        , updates :: [GroupEvent]
        }
    deriving (Show, Generic, FromJSON)

makeBaseURI :: MonadThrow m => PollServer -> m URI.URI
makeBaseURI PollServer {..} = do
    maybe ex pure . URI.parseURI $
        server <> "?act=a_check&key=" <> key <> "&wait=25"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte Long Poll URL"

getLongPollServer :: HTTP.Handle -> Config -> IO PollServer
getLongPollServer http Config {..} = do
    json <-
        http & HTTP.get $
        "https://api.vk.com/method/groups.getLongPollServer?v=" <>
        v <> "&access_token=" <> key <> "&group_id=" <> show group_id
    res <- throwDecode json
    pure $ response res

rememberLastUpdate :: Handle -> PollResponse -> IO PollResponse
rememberLastUpdate hAPI p@PollResponse {ts} = API.setState hAPI ts >> pure p

getUpdates :: Handle -> IO API.Request
getUpdates hAPI = do
    ts <- API.getState hAPI
    let uri = API.baseURI hAPI
    pure . API.GET $ URI.addQueryParams uri [("ts", Just $ show ts)]

data MessageNew =
    MessageNew
        { message :: Object
        , client_info :: Object
        }
    deriving (Show, Generic, FromJSON)

data GroupEvent
    = Message MessageNew
    | Other
    deriving (Show)

instance FromJSON GroupEvent where
    parseJSON =
        withObject "FromJSON API.Vkontakte" $ \o -> do
            String eventType <- o .: "type"
            case eventType of
                "message_new" -> Message <$> o .: "object"
                _ -> pure Other
