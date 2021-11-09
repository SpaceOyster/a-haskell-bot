{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module API.Vkontakte
    ( Handle(..)
    , new
    , Config(..)
    , VKState(..)
    , Method(..)
    , Response(..)
    , GroupEvent(..)
    , User(..)
    , Message(..)
    , Attachment(..)
    , CallbackEvent(..)
    , extractUpdates
    , Keyboard(..)
    , KeyboardButton(..)
    , ButtonColor(..)
    , KeyboardAction(..)
    , KeyboardActionType(..)
    ) where

import qualified API
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Function ((&))
import qualified Data.Hashable as H
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Exceptions as Ex
import GHC.Generics
import qualified HTTP
import qualified Logger
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

new :: Config -> Logger.Handle -> IO Handle
new cfg@Config {..} hLog = do
    http <- HTTP.new $ HTTP.Config {}
    baseURI <- makeBaseURI cfg
    apiState <- newIORef mempty
    let hAPI = API.Handle {..}
    initiatePollServer hAPI

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config hLog io = do
    hAPI <- new config hLog
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

data Response
    = Error
          { error_code :: Integer
          , error_msg :: String
          }
    | PollServ PollServer
    | PollResponse
          { ts :: String
          , updates :: [GroupEvent]
          }
    | PollError Integer
    | OtherResponse A.Object
    deriving (Show)

instance A.FromJSON Response where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte.Response" $ \o -> do
            errO <- o A..:? "error" A..!= mempty
            asum
                [ Error <$> errO A..: "error_code" <*> errO A..: "error_msg"
                , PollServ <$> o A..: "response"
                , PollError <$> o A..: "failed"
                , PollResponse <$> o A..: "ts" <*> o A..: "updates"
                , OtherResponse <$> o A..: "response"
                ]

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
        PollServ r -> pure r

rememberLastUpdate :: Handle -> Response -> IO Response
rememberLastUpdate hAPI p@PollResponse {ts} =
    API.modifyState hAPI (\s -> s {lastTS = ts}) >> pure p
rememberLastUpdate hAPI p = pure p

instance API.StatefullAPI (API.Handle VKState) where
    type Response (API.Handle VKState) = Response
    type Method (API.Handle VKState) = Method
    runMethod :: Handle -> Method -> IO Response
    runMethod hAPI m =
        rememberLastUpdate hAPI =<<
        throwDecode =<< API.sendRequest hAPI =<< runMethod' hAPI m

data Method
    = GetUpdates
    | SendMessageEventAnswer CallbackEvent String
    | SendTextMessage Integer String
    | CopyMessage Message
    | SendKeyboard Integer String Keyboard
    deriving (Show)

runMethod' :: Handle -> Method -> IO API.Request
runMethod' hAPI m =
    case m of
        GetUpdates -> getUpdates hAPI
        SendMessageEventAnswer ce prompt ->
            sendMessageEventAnswer hAPI ce prompt
        SendTextMessage peer_id text -> sendTextMessage hAPI peer_id text
        CopyMessage msg -> copyMessage hAPI msg
        SendKeyboard peer_id prompt keyboard ->
            sendKeyboard hAPI peer_id prompt keyboard

getUpdates :: Handle -> IO API.Request
getUpdates hAPI = do
    VKState {..} <- API.getState hAPI
    pure . API.GET $ URI.addQueryParams pollURI [("ts", Just lastTS)]

newtype User =
    User
        { unUser :: Integer
        }
    deriving (Show)

instance H.Hashable User where
    hash = unUser

data Message =
    Message
        { id :: Integer
        , date :: Integer
        , peer_id :: Integer
        , from_id :: Integer
        , text :: String
        , random_id :: Maybe Integer
        , attachments :: [Attachment]
        , payload :: Maybe A.Value
        , keyboard :: Maybe Keyboard
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
        Photo m -> "photo" <> mediaToQuery m
        Audio m -> "audio" <> mediaToQuery m
        Video m -> "video" <> mediaToQuery m
        Doc m -> "doc" <> mediaToQuery m
  where
    mediaToQuery :: MediaDoc -> String
    mediaToQuery MediaDoc {..} =
        show owner_id <> "_" <> show id <> maybe "" ('_' :) access_key

data CallbackEvent =
    CallbackEvent
        { user_id :: Integer
        , peer_id :: Integer
        , event_id :: String
        , payload :: A.Value
        , conversation_message_id :: Integer
        }
    deriving (Show, Generic, A.FromJSON)

data GroupEvent
    = MessageNew Message
    | MessageEvent CallbackEvent
    | Other
    deriving (Show)

instance A.FromJSON GroupEvent where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte.GroupEvent" $ \o -> do
            A.String eventType <- o A..: "type"
            case eventType of
                "message_new" -> MessageNew <$> o A..: "object"
                "message_event" -> MessageEvent <$> o A..: "object"
                _ -> fail "Unknown GroupEvent type"

sendMessageEventAnswer ::
       (Monad m) => Handle -> CallbackEvent -> String -> m API.Request
sendMessageEventAnswer hAPI CallbackEvent {..} prompt =
    pure . API.GET . apiMethod hAPI "messages.sendMessageEventAnswer" $
    [ ("event_id", Just event_id)
    , ("user_id", Just $ show user_id)
    , ("peer_id", Just $ show peer_id)
    , ( "event_data"
      , Just $
        L8.unpack $
        A.encode $
        A.object ["type" A..= ("show_snackbar" :: String), "text" A..= prompt])
    ]

apiMethod :: Handle -> String -> URI.QueryParams -> URI.URI
apiMethod hAPI method qps =
    flip URI.addQueryParams qps . URI.addPath (API.baseURI hAPI) $ method

sendMessageWith :: Handle -> Integer -> String -> URI.QueryParams -> API.Request
sendMessageWith hAPI peer_id text qps =
    API.GET . apiMethod hAPI "messages.send" $
    [("peer_id", Just $ show peer_id), ("message", Just text)] <> qps

sendTextMessage :: (Monad m) => Handle -> Integer -> String -> m API.Request
sendTextMessage hAPI peer_id text =
    pure $ sendMessageWith hAPI peer_id text mempty

copyMessage :: (Monad m) => Handle -> Message -> m API.Request
copyMessage hAPI Message {..} =
    pure $
    sendMessageWith hAPI peer_id text $ fmap attachmentToQuery attachments

extractUpdates :: (MonadThrow m) => Response -> m [GroupEvent]
extractUpdates PollResponse {..} = pure updates
extractUpdates (PollError c) = throwM $ Ex.VKPollError $ show c

data Keyboard =
    Keyboard
        { one_time :: Bool
        , buttons :: [[KeyboardButton]]
        , inline :: Bool
        }
    deriving (Show, Generic, A.ToJSON, A.FromJSON)

data KeyboardButton =
    KeyboardButton
        { action :: KeyboardAction
        , color :: ButtonColor
        }
    deriving (Show, Generic, A.ToJSON, A.FromJSON)

data ButtonColor
    = Primary
    | Secondary
    | Negative
    | Positive
    deriving (Show, Generic)

instance A.ToJSON ButtonColor where
    toJSON = A.genericToJSON options
      where
        options = A.defaultOptions {A.constructorTagModifier = fmap toLower} -- TODO do better

instance A.FromJSON ButtonColor where
    parseJSON =
        A.withText "FromJSON API.Vkontakte.ButtonColor" $ \t ->
            case t of
                "primary" -> pure Primary
                "secondary" -> pure Secondary
                "negative" -> pure Negative
                "positive" -> pure Positive
                _ -> fail "Unknown color"

data KeyboardAction =
    KeyboardAction
        { action_type :: KeyboardActionType
        , label :: Maybe String
        , payload :: Maybe A.Value
        , link :: Maybe String
        }
    deriving (Show, Generic)

instance A.ToJSON KeyboardAction where
    toJSON KeyboardAction {..} =
        A.object $
        filter
            (\(_, mv) -> mv /= A.Null) -- TODO do better
            [ "type" A..= action_type
            , "label" A..= label
            , "payload" A..= payload
            , "link" A..= link
            ]

instance A.FromJSON KeyboardAction where
    parseJSON =
        A.withObject "FromJSON API.Vkontakte.KeyboardAction" $ \o -> do
            action_type <- o A..: "type"
            label <- o A..:? "label"
            payload <- o A..:? "payload"
            link <- o A..:? "link"
            pure KeyboardAction {..}

data KeyboardActionType
    = Text
    | OpenLink
    | Location
    | Callback
    deriving (Show, Generic)

instance A.ToJSON KeyboardActionType where
    toJSON = A.genericToJSON options
      where
        options = A.defaultOptions {A.constructorTagModifier = fmap toLower}

instance A.FromJSON KeyboardActionType where
    parseJSON =
        A.withText "FromJSON API.Vkontakte.KeyboardActionType" $ \t ->
            case t of
                "text" -> pure Text
                "openlink" -> pure OpenLink
                "location" -> pure Location
                "callback" -> pure Callback
                _ -> fail "Unknown Action Type"

sendKeyboard ::
       (Monad m) => Handle -> Integer -> String -> Keyboard -> m API.Request
sendKeyboard hAPI peer_id prompt keyboard =
    pure . sendMessageWith hAPI peer_id prompt $
    [("keyboard", Just $ L8.unpack $ A.encode keyboard)]
