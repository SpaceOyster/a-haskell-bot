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

data VKState =
    VKState
        { lastTS :: String
        , pollURI :: URI.URI
        }

type Handle = API.Handle VKState

data Config =
    Config
        { key :: String
        , group_id :: Integer
        , v :: String
        }

instance Semigroup VKState where
    a <> b = b

instance Monoid VKState where
    mempty = VKState {lastTS = mempty, pollURI = URI.nullURI}

new :: Config -> IO Handle
new cfg@Config {..} = do
    http <- HTTP.new $ HTTP.Config {}
    baseURI <- makeBaseURI cfg
    apiState <- newIORef mempty
    let hAPI = API.Handle {http, hLog = undefined, baseURI, apiState}
    initiatePollServer hAPI

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config io = do
    hAPI <- new config
    io hAPI

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
    maybe ex pure . URI.parseURI $
    "https://api.vk.com/method/?v=" <>
    v <> "&access_token=" <> key <> "&group_id=" <> show group_id
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte API URL"

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

initiatePollServer :: Handle -> IO Handle
initiatePollServer hAPI = do
    ps@PollServer {ts} <- getLongPollServer hAPI
    pollURI <- makePollURI ps
    API.setState hAPI $ VKState {lastTS = ts, pollURI}
    pure hAPI

makePollURI :: MonadThrow m => PollServer -> m URI.URI
makePollURI PollServer {..} = do
    maybe ex pure . URI.parseURI $
        server <> "?act=a_check&key=" <> key <> "&wait=25"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte Long Poll URL"

getLongPollServer :: Handle -> IO PollServer
getLongPollServer hAPI = do
    json <-
        API.sendRequest hAPI $
        API.GET $ apiMethod hAPI "groups.getLongPollServer" mempty
    res <- throwDecode json
    pure $ response res

rememberLastUpdate :: Handle -> PollResponse -> IO PollResponse
rememberLastUpdate hAPI p@PollResponse {ts} =
    API.modifyState hAPI (\s -> s {lastTS = ts}) >> pure p

getUpdates :: Handle -> IO API.Request
getUpdates hAPI = do
    VKState {..} <- API.getState hAPI
    let uri = API.baseURI hAPI
    pure . API.GET $ URI.addQueryParams uri [("ts", Just lastTS)]

data Message =
    Message
        { id :: Integer
        , date :: Integer
        , peer_id :: Integer
        , from_id :: Integer
        , text :: String
        , random_id :: Maybe Integer
        , attachements :: [Value]
        , payload :: Maybe String
        , keyboard :: Maybe Object
        , is_cropped :: Maybe Bool
        }
    deriving (Show, Generic, FromJSON)

data GroupEvent
    = MessageNew Message
    | Other
    deriving (Show)

instance FromJSON GroupEvent where
    parseJSON =
        withObject "FromJSON API.Vkontakte" $ \o -> do
            String eventType <- o .: "type"
            case eventType of
                "message_new" -> MessageNew <$> o .: "object"
                _ -> pure Other

apiMethod :: Handle -> String -> URI.QueryParams -> URI.URI
apiMethod hAPI method qps =
    flip URI.addQueryParams qps . URI.addPath (API.baseURI hAPI) $ method

