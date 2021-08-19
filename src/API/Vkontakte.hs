{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Vkontakte
    ( Handle(..)
    , new
    , withHandle
    , Config(..)
    , PollResponse(..)
    , Message(..)
    , Attachment(..)
    , getUpdates
    , rememberLastUpdate
    , copyMessage
    , GroupEvent(..)
    , VKState(..)
    , extractUpdates
    ) where

import qualified API
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Aeson as A
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
    let hLog = undefined
    http <- HTTP.new $ HTTP.Config {}
    baseURI <- makeBaseURI cfg
    apiState <- newIORef mempty
    let hAPI = API.Handle {..}
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
    deriving (Show, Generic, A.FromJSON)

data APIResponse a
    = Error
          { error_code :: Integer
          , error_msg :: String
          }
    | Response a

instance (A.FromJSON a) => A.FromJSON (APIResponse a) where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte.APIResponse" $ \o -> do
            errO <- o A..: "error"
            let err = Error <$> errO A..: "error_code" <*> errO A..: "error_msg"
            let resp = Response <$> o A..: "response"
            err <|> resp

data PollResponse
    = PollResponse
          { ts :: String
          , updates :: [GroupEvent]
          }
    | PollError Integer
    deriving (Show)

instance A.FromJSON PollResponse where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte.PollResponse" $ \o -> do
            let err = PollError <$> o A..: "failed"
            let resp = PollResponse <$> o A..: "ts" <*> o A..: "updates"
            resp <|> err

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
    case res of
        Error {..} ->
            throwM $
            Ex.APIRespondedWithError $ show error_code <> ": " <> error_msg
        Response r -> pure r

rememberLastUpdate :: Handle -> PollResponse -> IO PollResponse
rememberLastUpdate hAPI p@PollResponse {ts} =
    API.modifyState hAPI (\s -> s {lastTS = ts}) >> pure p
rememberLastUpdate hAPI p = pure p

getUpdates :: Handle -> IO API.Request
getUpdates hAPI = do
    VKState {..} <- API.getState hAPI
    pure . API.GET $ URI.addQueryParams pollURI [("ts", Just lastTS)]

data Message =
    Message
        { id :: Integer
        , date :: Integer
        , peer_id :: Integer
        , from_id :: Integer
        , text :: String
        , random_id :: Maybe Integer
        , attachments :: [Attachment]
        , payload :: Maybe String
        , keyboard :: Maybe A.Object
        , is_cropped :: Maybe Bool
        }
    deriving (Show, Generic, A.FromJSON)

data MediaDoc =
    MediaDoc
        { id :: Integer
        , owner_id :: Integer
        , access_key :: Maybe String
        }
    deriving (Show, Generic, A.FromJSON)

data Attachment
    = Photo MediaDoc
    | Audio MediaDoc
    | Video MediaDoc
    | Doc MediaDoc
    | Sticker
          { product_id :: Integer
          , sticker_id :: Integer
          }
    | OtherA
    deriving (Show)

instance A.FromJSON Attachment where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte Attachment" $ \o -> do
            A.String media_type <- o A..: "type"
            o' <- o A..: media_type
            case media_type of
                "sticker" ->
                    Sticker <$> o' A..: "product_id" <*> o' A..: "sticker_id"
                "photo" -> Photo <$> o A..: media_type
                "audio" -> Audio <$> o A..: media_type
                "video" -> Video <$> o A..: media_type
                "doc" -> Doc <$> o A..: media_type
                _ -> pure OtherA

attachmentToQuery :: Attachment -> URI.QueryParam
attachmentToQuery OtherA = ("", Nothing)
attachmentToQuery Sticker {..} = ("sticker_id", Just $ show sticker_id)
attachmentToQuery a =
    (,) "attachment" . Just $
    case a of
        Photo m -> "photo_" <> mediaToQuery m
        Audio m -> "audio_" <> mediaToQuery m
        Video m -> "video_" <> mediaToQuery m
        Doc m -> "doc_" <> mediaToQuery m
  where
    mediaToQuery :: MediaDoc -> String
    mediaToQuery MediaDoc {..} =
        show id <> "_" <> show owner_id <> maybe "" ('_' :) access_key

data GroupEvent
    = MessageNew Message
    | Other
    deriving (Show)

instance A.FromJSON GroupEvent where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte.GroupEvent" $ \o -> do
            A.String eventType <- o A..: "type"
            case eventType of
                "message_new" -> MessageNew <$> o A..: "object"
                _ -> pure Other

apiMethod :: Handle -> String -> URI.QueryParams -> URI.URI
apiMethod hAPI method qps =
    flip URI.addQueryParams qps . URI.addPath (API.baseURI hAPI) $ method

copyMessage :: (Monad m) => Handle -> Message -> m API.Request
copyMessage hAPI Message {..} =
    pure . API.GET . apiMethod hAPI "messages.send" $
    [("peer_id", Just $ show peer_id), ("message", Just $ show text)] <>
    fmap attachmentToQuery attachments

extractUpdates :: (MonadThrow m) => PollResponse -> m [GroupEvent]
extractUpdates PollResponse {..} = pure updates
extractUpdates (PollError c) = throwM $ Ex.VKPollError $ show c
