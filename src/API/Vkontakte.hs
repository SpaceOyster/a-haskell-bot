{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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
    , runMethod
    ) where

import App.Monad
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower)
import Data.Foldable (asum)
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text.Extended as T
import qualified Exceptions as Ex
import GHC.Generics
import qualified HTTP
import qualified Logger
import qualified Network.URI.Extended as URI

data VKState =
    VKState
        { lastTS :: String
        , pollURI :: URI.URI
        }
    deriving (Show)

data Handle =
    Handle
        { baseURI :: URI.URI
        , apiState :: IORef VKState
        }

getState :: (MonadIO m) => Handle -> m VKState
getState = liftIO . readIORef . apiState

modifyState :: (MonadIO m) => Handle -> (VKState -> VKState) -> m ()
modifyState hAPI morph = liftIO $ apiState hAPI `modifyIORef'` morph

setState :: (MonadIO m) => Handle -> VKState -> m ()
setState hAPI newState = modifyState hAPI $ const newState

data Config =
    Config
        { key :: String
        , group_id :: Integer
        , v :: String
        }

instance Semigroup VKState where
    _a <> b = b

instance Monoid VKState where
    mempty = VKState {lastTS = mempty, pollURI = URI.nullURI}

new :: Config -> Logger.Handle -> HTTP.Handle -> IO Handle
new cfg hLog hHTTP = do
    Logger.logInfo hLog "Initiating Vkontakte API handle"
    baseURI <- makeBaseURI cfg
    apiState <- newIORef mempty
    let hAPI = Handle {baseURI, apiState}
    initiatePollServer hAPI hHTTP

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
    | OtherResponse A.Value
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

initiatePollServer ::
       (MonadIO m, MonadThrow m) => Handle -> HTTP.Handle -> m Handle
initiatePollServer hAPI hHTTP = do
    ps@PollServer {ts} <- getLongPollServer hAPI hHTTP
    pollURI <- makePollURI ps
    let pollCreds = VKState {lastTS = ts, pollURI}
    setState hAPI pollCreds
    pure hAPI

makePollURI :: MonadThrow m => PollServer -> m URI.URI
makePollURI PollServer {key, server} = do
    maybe ex pure . URI.parseURI $
        server <> "?act=a_check&key=" <> key <> "&wait=25"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte Long Poll URL"

getLongPollServer ::
       (MonadIO m, MonadThrow m) => Handle -> HTTP.Handle -> m PollServer
getLongPollServer hAPI hHTTP = do
    let req = HTTP.GET $ apiMethod hAPI "groups.getLongPollServer" mempty
    json <- liftIO $ HTTP.sendRequest hHTTP req
    res <- A.throwDecode json
    case res of
        Error {error_code, error_msg} ->
            throwM $
            Ex.APIRespondedWithError $ show error_code <> ": " <> error_msg
        PollServ r -> pure r
        _ -> throwM $ Ex.APIRespondedWithError "Expected PollServer object"

rememberLastUpdate ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has Logger.Handle env)
    => Handle
    -> Response
    -> m Response
rememberLastUpdate hAPI res = modifyState hAPI (updateStateWith res) >> pure res

updateStateWith :: Response -> (VKState -> VKState)
updateStateWith PollResponse {ts} = \s -> s {lastTS = ts}
updateStateWith _ = id

runMethod ::
       ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has Logger.Handle env
       , Has HTTP.Handle env
       )
    => Handle
    -> Method
    -> m Response
runMethod hAPI m = do
    state <- getState hAPI
    envLogDebug $ "last recieved Update TS: " <> T.tshow (lastTS state)
    let req = mkRequest hAPI state m
    hHTTP <- grab @HTTP.Handle
    (liftIO $ HTTP.sendRequest hHTTP req) >>= A.throwDecode >>=
        rememberLastUpdate hAPI

data Method
    = GetUpdates
    | SendMessageEventAnswer CallbackEvent String
    | SendTextMessage Integer String
    | CopyMessage Message
    | SendKeyboard Integer String Keyboard
    deriving (Show)

mkRequest :: Handle -> VKState -> Method -> HTTP.Request
mkRequest hAPI s m =
    case m of
        GetUpdates -> getUpdates hAPI s
        SendMessageEventAnswer ce prompt ->
            sendMessageEventAnswer hAPI ce prompt
        SendTextMessage peer_id text -> sendTextMessage hAPI peer_id text
        CopyMessage msg -> copyMessage hAPI msg
        SendKeyboard peer_id prompt keyboard ->
            sendKeyboard hAPI peer_id prompt keyboard

getUpdates :: Handle -> VKState -> HTTP.Request
getUpdates _hAPI VKState {..} =
    HTTP.GET $ URI.addQueryParams pollURI [("ts", Just lastTS)]

newtype User =
    User
        { unUser :: Integer
        }
    deriving (Show)

instance H.Hashable User where
    hash = unUser

data Message =
    Message
        { msg_id :: Integer
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
    deriving (Show)

instance A.FromJSON Message where
    parseJSON =
        A.withObject "Message" $ \o -> do
            msg_id <- o A..: "id"
            date <- o A..: "date"
            peer_id <- o A..: "peer_id"
            from_id <- o A..: "from_id"
            text <- o A..: "text"
            random_id <- o A..:? "random_id"
            attachments <- o A..: "attachments"
            payload <- o A..:? "payload"
            keyboard <- o A..:? "keyboard"
            is_cropped <- o A..:? "is_cropped"
            pure $ Message {..}

data MediaDoc =
    MediaDoc
        { mdoc_id :: Integer
        , owner_id :: Integer
        , access_key :: Maybe String
        }
    deriving (Show)

instance A.FromJSON MediaDoc where
    parseJSON =
        A.withObject "MediaDoc" $ \o -> do
            mdoc_id <- o A..: "id"
            owner_id <- o A..: "owner_id"
            access_key <- o A..: "access_key"
            pure $ MediaDoc {..}

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
        Sticker {sticker_id} -> "sticker_id=" <> show sticker_id
        OtherA -> mempty
  where
    mediaToQuery :: MediaDoc -> String
    mediaToQuery MediaDoc {..} =
        show owner_id <> "_" <> show mdoc_id <> maybe "" ('_' :) access_key

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

sendMessageEventAnswer :: Handle -> CallbackEvent -> String -> HTTP.Request
sendMessageEventAnswer hAPI CallbackEvent {..} prompt =
    HTTP.GET . apiMethod hAPI "messages.sendMessageEventAnswer" $
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
    flip URI.addQueryParams qps . URI.addPath (baseURI hAPI) $ method

sendMessageWith ::
       Handle -> Integer -> String -> URI.QueryParams -> HTTP.Request
sendMessageWith hAPI peer_id text qps =
    HTTP.GET . apiMethod hAPI "messages.send" $
    [("peer_id", Just $ show peer_id), ("message", Just text)] <> qps

sendTextMessage :: Handle -> Integer -> String -> HTTP.Request
sendTextMessage hAPI peer_id text = sendMessageWith hAPI peer_id text mempty

copyMessage :: Handle -> Message -> HTTP.Request
copyMessage hAPI Message {..} =
    sendMessageWith hAPI peer_id text $ fmap attachmentToQuery attachments

extractUpdates :: (MonadThrow m) => Response -> m [GroupEvent]
extractUpdates PollResponse {..} = pure updates
extractUpdates (PollError c) = throwM $ Ex.VKPollError $ show c
extractUpdates _ = throwM $ Ex.VKPollError "Expexted PollResponse"

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
        A.withText "FromJSON API.Vkontakte.ButtonColor" $ \case
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
        A.withText "FromJSON API.Vkontakte.KeyboardActionType" $ \case
            "text" -> pure Text
            "openlink" -> pure OpenLink
            "location" -> pure Location
            "callback" -> pure Callback
            _ -> fail "Unknown Action Type"

sendKeyboard :: Handle -> Integer -> String -> Keyboard -> HTTP.Request
sendKeyboard hAPI peer_id prompt keyboard =
    sendMessageWith
        hAPI
        peer_id
        prompt
        [("keyboard", Just $ L8.unpack $ A.encode keyboard)]
