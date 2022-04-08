{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Vkontakte.Methods where

import API.Vkontakte.Monad
import API.Vkontakte.Types
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (get, modify')
import Control.Monad.Trans (MonadTrans (..), lift)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

runMethod ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Method ->
  VkontakteT m Response
runMethod m = do
  st <- get
  lift $ Log.logDebug $ "last recieved Update TS: " <> T.tshow (lastTS st)
  req <- mkRequest st m
  json <- lift $ HTTP.sendRequest req
  lift $ Log.logDebug $ "Got response: " <> T.lazyDecodeUtf8 json
  maybe (ex json) rememberLastUpdate $ A.decode json
  where
    ex json = throwM $ apiError $ "Unexpected response: " <> T.lazyDecodeUtf8 json

data Method
  = GetUpdates
  | SendMessageEventAnswer CallbackEvent T.Text
  | SendTextMessage Integer T.Text
  | CopyMessage Message
  | SendKeyboard Integer T.Text Keyboard
  deriving (Show)

mkRequest :: (Monad m) => VKState -> Method -> VkontakteT m HTTP.Request
mkRequest st m =
  case m of
    GetUpdates -> getUpdates_ st
    SendMessageEventAnswer ce prompt -> sendMessageEventAnswer_ st ce prompt
    SendTextMessage peer_id text -> sendTextMessage_ st peer_id text
    CopyMessage msg -> copyMessage_ st msg
    SendKeyboard peer_id prompt keyboard ->
      sendKeyboard_ st peer_id prompt keyboard

getUpdates :: (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => VkontakteT m [GroupEvent]
getUpdates = runMethod GetUpdates >>= extractUpdates

sendMessageEventAnswer :: (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => CallbackEvent -> T.Text -> VkontakteT m Response
sendMessageEventAnswer cbE prompt = runMethod $ SendMessageEventAnswer cbE prompt

sendTextMessage :: (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Integer -> T.Text -> VkontakteT m Response
sendTextMessage peer_id text = runMethod $ SendTextMessage peer_id text

copyMessage :: (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Message -> VkontakteT m Response
copyMessage msg = runMethod $ CopyMessage msg

sendKeyboard :: (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Integer -> T.Text -> Keyboard -> VkontakteT m Response
sendKeyboard peer_id prompt kbd = runMethod $ SendKeyboard peer_id prompt kbd

getUpdates_ :: (Monad m) => VKState -> VkontakteT m HTTP.Request
getUpdates_ VKState {..} =
  pure . HTTP.GET $ URI.addQueryParams pollURI ["ts" URI.:=: T.unpack lastTS, "wait" URI.:=: "25"]

sendMessageEventAnswer_ :: (Monad m) => VKState -> CallbackEvent -> T.Text -> VkontakteT m HTTP.Request
sendMessageEventAnswer_ st CallbackEvent {..} prompt =
  fmap HTTP.GET $
    apiMethod st "messages.sendMessageEventAnswer" $
      [ "event_id" URI.:=: T.unpack event_id,
        "user_id" URI.:=: show user_id,
        "peer_id" URI.:=: show peer_id,
        "event_data" URI.:=: L8.unpack . A.encode $ mkJSON prompt
      ]
  where
    mkJSON :: T.Text -> A.Value
    mkJSON p = A.object ["type" A..= ("show_snackbar" :: T.Text), "text" A..= p]

sendMessageWith ::
  (Monad m) => VKState -> Integer -> T.Text -> [URI.QueryParam] -> VkontakteT m HTTP.Request
sendMessageWith st peer_id text qps =
  fmap HTTP.GET $
    apiMethod st "messages.send" $
      ["peer_id" URI.:=: show peer_id, "message" URI.:=: T.unpack text] <> qps

sendTextMessage_ :: (Monad m) => VKState -> Integer -> T.Text -> VkontakteT m HTTP.Request
sendTextMessage_ st peer_id text = sendMessageWith st peer_id text mempty

copyMessage_ :: (Monad m) => VKState -> Message -> VkontakteT m HTTP.Request
copyMessage_ st Message {..} =
  sendMessageWith st peer_id text $ fmap attachmentToQuery attachments

sendKeyboard_ :: Monad m => VKState -> Integer -> T.Text -> Keyboard -> VkontakteT m HTTP.Request
sendKeyboard_ st peer_id prompt kbd =
  sendMessageWith st peer_id prompt ["keyboard" URI.:=: L8.unpack $ A.encode kbd]
