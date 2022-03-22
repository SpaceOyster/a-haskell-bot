{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module API.Vkontakte
  ( initiate,
    VkontakteT (..),
    Config (..),
    VKState (..),
    Method (..),
    Response (..),
    GroupEvent (..),
    User (..),
    Message (..),
    Attachment (..),
    CallbackEvent (..),
    extractUpdates,
    Keyboard (..),
    KeyboardButton (..),
    ButtonColor (..),
    KeyboardAction (..),
    KeyboardActionType (..),
    runMethod,
  )
where

import qualified App.Error as Ex
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.State (MonadState (..), StateT, get, modify')
import Control.Monad.Trans (MonadTrans (..), lift)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (toLower)
import Data.Foldable (asum)
import qualified Data.Hashable as H
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import GHC.Generics (Generic)
import qualified Network.URI.Extended as URI

data VKState = VKState
  { lastTS :: T.Text,
    pollURI :: URI.URI,
    apiURI :: URI.URI
  }
  deriving (Show)

newtype VkontakteT m a = VkontakteT {unVkontakteT :: StateT VKState m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadState VKState
    )

instance MonadTrans VkontakteT where
  lift = VkontakteT . lift

data Config = Config
  { key :: String,
    group_id :: Integer,
    v :: String
  }

instance Semigroup VKState where
  _a <> b = b

instance Monoid VKState where
  mempty =
    VKState {lastTS = mempty, pollURI = URI.nullURI, apiURI = URI.nullURI}

initiate ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Config -> m VKState
initiate cfg = do
  Log.logInfo "Initiating Vkontakte API handle"
  apiURI <- makeBaseURI cfg
  initiatePollServer mempty {apiURI}

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI $
    "https://api.vk.com/method/?v=" <> v
      <> "&access_token="
      <> key
      <> "&group_id="
      <> show group_id
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte API URL"

data PollServer = PollServer
  { key :: T.Text,
    server :: T.Text,
    ts :: T.Text
  }
  deriving (Show, Generic, A.FromJSON)

data Error = Error
  { error_code :: Integer,
    error_msg :: T.Text
  }
  deriving (Show, Generic, A.FromJSON)

data Poll = Poll
  { ts :: T.Text,
    updates :: [GroupEvent]
  }
  deriving (Show, Generic, A.FromJSON)

data Response
  = ErrorResponse Error
  | PollResponse Poll
  | PollError Integer
  | OtherResponse A.Value
  deriving (Show)

instance A.FromJSON Response where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.Response" $ \o -> do
      errO <- o A..:? "error" A..!= mempty
      asum
        [ ErrorResponse <$> A.parseJSON (A.Object errO),
          PollError <$> o A..: "failed",
          PollResponse <$> A.parseJSON (A.Object o),
          OtherResponse <$> o A..: "response"
        ]

data PollInitResponse
  = PollInitServer PollServer
  | PollInitError Error
  deriving (Show)

instance A.FromJSON PollInitResponse where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.PollInitResponse" $ \o -> do
      errO <- o A..:? "error" A..!= mempty
      asum
        [ PollInitError <$> A.parseJSON (A.Object errO),
          PollInitServer <$> o A..: "response"
        ]

initiatePollServer :: (MonadThrow m, HTTP.MonadHTTP m) => VKState -> m VKState
initiatePollServer st = do
  ps@PollServer {ts} <- getLongPollServer st
  pollURI <- makePollURI ps
  let pollCreds = st {lastTS = ts, pollURI}
  pure pollCreds

makePollURI :: MonadThrow m => PollServer -> m URI.URI
makePollURI PollServer {key, server} = do
  maybe ex pure . URI.parseURI $
    T.unpack server <> "?act=a_check&key="
      <> T.unpack key
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte Long Poll URL"

getLongPollServer :: (MonadThrow m, HTTP.MonadHTTP m) => VKState -> m PollServer
getLongPollServer st = do
  let req = HTTP.GET $ apiMethod st "groups.getLongPollServer" mempty
  json <- HTTP.sendRequest req
  case A.decode json of
    Just (PollInitServer r) -> pure r
    Just (PollInitError Error {error_code, error_msg}) ->
      throwM $
        Ex.APIRespondedWithError $
          show error_code <> ": " <> T.unpack error_msg
    Nothing -> throwM $ Ex.apiUnexpectedResponse $ T.lazyDecodeUtf8 json

rememberLastUpdate ::
  (MonadThrow m, Log.MonadLog m) => Response -> VkontakteT m Response
rememberLastUpdate res = modify' (updateStateWith res) >> pure res

updateStateWith :: Response -> (VKState -> VKState)
updateStateWith (PollResponse poll) = \s -> s {lastTS = ts (poll :: Poll)}
updateStateWith _ = id

runMethod ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Method ->
  VkontakteT m Response
runMethod m = do
  st <- get
  lift $ Log.logDebug $ "last recieved Update TS: " <> T.tshow (lastTS st)
  let req = mkRequest st m
  json <- lift $ HTTP.sendRequest req
  lift $ Log.logDebug $ "Got response: " <> T.lazyDecodeUtf8 json
  maybe (ex json) rememberLastUpdate $ A.decode json
  where
    ex json = throwM $ Ex.apiUnexpectedResponse $ T.lazyDecodeUtf8 json

data Method
  = GetUpdates
  | SendMessageEventAnswer CallbackEvent T.Text
  | SendTextMessage Integer T.Text
  | CopyMessage Message
  | SendKeyboard Integer T.Text Keyboard
  deriving (Show)

mkRequest :: VKState -> Method -> HTTP.Request
mkRequest st m =
  case m of
    GetUpdates -> getUpdates st
    SendMessageEventAnswer ce prompt -> sendMessageEventAnswer st ce prompt
    SendTextMessage peer_id text -> sendTextMessage st peer_id text
    CopyMessage msg -> copyMessage st msg
    SendKeyboard peer_id prompt keyboard ->
      sendKeyboard st peer_id prompt keyboard

getUpdates :: VKState -> HTTP.Request
getUpdates VKState {..} =
  HTTP.GET $ URI.addQueryParams pollURI ["ts" URI.:=: T.unpack lastTS, "wait" URI.:=: "25"]

newtype User = User
  { unUser :: Integer
  }
  deriving (Show)

instance H.Hashable User where
  hash = unUser

data Message = Message
  { msg_id :: Integer,
    date :: Integer,
    peer_id :: Integer,
    from_id :: Integer,
    text :: T.Text,
    random_id :: Maybe Integer,
    attachments :: [Attachment],
    payload :: Maybe A.Value,
    keyboard :: Maybe Keyboard,
    is_cropped :: Maybe Bool
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

data MediaDoc = MediaDoc
  { mdoc_id :: Integer,
    owner_id :: Integer,
    access_key :: Maybe T.Text
  }
  deriving (Show)

instance A.FromJSON MediaDoc where
  parseJSON =
    A.withObject "MediaDoc" $ \o -> do
      mdoc_id <- o A..: "id"
      owner_id <- o A..: "owner_id"
      access_key <- o A..: "access_key"
      pure $ MediaDoc {..}

data Sticker = Sticker
  { product_id :: Integer,
    sticker_id :: Integer
  }
  deriving (Show, Generic, A.FromJSON)

data Attachment
  = Photo MediaDoc
  | Audio MediaDoc
  | Video MediaDoc
  | Doc MediaDoc
  | StickerA Sticker
  deriving (Show)

instance A.FromJSON Attachment where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte Attachment" $ \o -> do
      A.String media_type <- o A..: "type"
      case media_type of
        "sticker" -> StickerA <$> o A..: media_type
        "photo" -> Photo <$> o A..: media_type
        "audio" -> Audio <$> o A..: media_type
        "video" -> Video <$> o A..: media_type
        "doc" -> Doc <$> o A..: media_type
        _ -> fail "Expected Attachment"

attachmentToQuery :: Attachment -> URI.QueryParam
attachmentToQuery (StickerA s) = "sticker_id" URI.:=: show $ sticker_id s
attachmentToQuery a =
  "attachment"
    URI.:=: case a of
      Photo m -> "photo" <> mediaToQuery m
      Audio m -> "audio" <> mediaToQuery m
      Video m -> "video" <> mediaToQuery m
      Doc m -> "doc" <> mediaToQuery m
      StickerA s -> "sticker_id=" <> show (sticker_id s)
  where
    mediaToQuery :: MediaDoc -> String
    mediaToQuery MediaDoc {..} =
      show owner_id <> "_" <> show mdoc_id
        <> maybe "" ('_' :) (T.unpack <$> access_key)

data CallbackEvent = CallbackEvent
  { user_id :: Integer,
    peer_id :: Integer,
    event_id :: T.Text,
    payload :: A.Value,
    conversation_message_id :: Integer
  }
  deriving (Show, Generic, A.FromJSON)

data GroupEvent
  = MessageNew Message
  | MessageEvent CallbackEvent
  deriving (Show)

instance A.FromJSON GroupEvent where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.GroupEvent" $ \o -> do
      A.String eventType <- o A..: "type"
      case eventType of
        "message_new" -> MessageNew <$> o A..: "object"
        "message_event" -> MessageEvent <$> o A..: "object"
        _ -> fail "Unknown GroupEvent type"

sendMessageEventAnswer :: VKState -> CallbackEvent -> T.Text -> HTTP.Request
sendMessageEventAnswer st CallbackEvent {..} prompt =
  HTTP.GET . apiMethod st "messages.sendMessageEventAnswer" $
    [ "event_id" URI.:=: T.unpack event_id,
      "user_id" URI.:=: show user_id,
      "peer_id" URI.:=: show peer_id,
      "event_data" URI.:=: L8.unpack . A.encode $ mkJSON prompt
    ]
  where
    mkJSON :: T.Text -> A.Value
    mkJSON p = A.object ["type" A..= ("show_snackbar" :: T.Text), "text" A..= p]

apiMethod :: VKState -> T.Text -> [URI.QueryParam] -> URI.URI
apiMethod st method qps =
  flip URI.addQueryParams qps . URI.addPath (apiURI st) $ T.unpack method

sendMessageWith ::
  VKState -> Integer -> T.Text -> [URI.QueryParam] -> HTTP.Request
sendMessageWith st peer_id text qps =
  HTTP.GET . apiMethod st "messages.send" $
    ["peer_id" URI.:=: show peer_id, "message" URI.:=: T.unpack text]
      <> qps

sendTextMessage :: VKState -> Integer -> T.Text -> HTTP.Request
sendTextMessage st peer_id text = sendMessageWith st peer_id text mempty

copyMessage :: VKState -> Message -> HTTP.Request
copyMessage st Message {..} =
  sendMessageWith st peer_id text $ fmap attachmentToQuery attachments

extractUpdates :: (MonadThrow m) => Response -> m [GroupEvent]
extractUpdates (PollResponse poll) = pure $ updates poll
extractUpdates (PollError c) = throwM $ Ex.VKPollError $ show c
extractUpdates _ = throwM $ Ex.VKPollError "Expexted PollResponse"

data Keyboard = Keyboard
  { one_time :: Bool,
    buttons :: [[KeyboardButton]],
    inline :: Bool
  }
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data KeyboardButton = KeyboardButton
  { action :: KeyboardAction,
    color :: ButtonColor
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

data KeyboardAction = KeyboardAction
  { action_type :: KeyboardActionType,
    label :: Maybe T.Text,
    payload :: Maybe A.Value,
    link :: Maybe T.Text
  }
  deriving (Show, Generic)

instance A.ToJSON KeyboardAction where
  toJSON KeyboardAction {..} =
    A.object $
      filter
        ((/= A.Null) . snd) -- TODO do better
        [ "type" A..= action_type,
          "label" A..= label,
          "payload" A..= payload,
          "link" A..= link
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

sendKeyboard :: VKState -> Integer -> T.Text -> Keyboard -> HTTP.Request
sendKeyboard st peer_id prompt keyboard =
  sendMessageWith st peer_id prompt $
    ["keyboard" URI.:=: L8.unpack $ A.encode keyboard]
