{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Vkontakte.Methods
  ( getUpdates,
    sendMessageEventAnswer,
    sendTextMessage,
    copyMessage,
    sendKeyboard,
  )
where

import API.Vkontakte.Monad
import API.Vkontakte.Types
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (get)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

runMethod ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, A.FromJSON a) =>
  Method ->
  VkontakteT m a
runMethod m = do
  req <- mkRequest m
  json <- HTTP.sendRequest req
  Log.logDebug $ "Got response: " <> T.lazyDecodeUtf8 json
  maybe (ex json) fromResponse $ A.decode json
  where
    ex json = throwM $ apiError $ "Unexpected response: " <> T.lazyDecodeUtf8 json

data Method
  = SendMessageEventAnswer CallbackEvent T.Text
  | SendTextMessage Integer T.Text
  | CopyMessage Message
  | SendKeyboard Integer T.Text Keyboard
  deriving (Show)

mkRequest :: (Monad m) => Method -> VkontakteT m HTTP.Request
mkRequest m =
  case m of
    SendMessageEventAnswer ce prompt -> sendMessageEventAnswer_ ce prompt
    SendTextMessage peer_id text -> sendTextMessage_ peer_id text
    CopyMessage msg -> copyMessage_ msg
    SendKeyboard peer_id prompt keyboard ->
      sendKeyboard_ peer_id prompt keyboard

getUpdates :: (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => VkontakteT m [GroupEvent]
getUpdates = do
  st <- get
  Log.logDebug $ "last recieved Update TS: " <> T.tshow (lastTS st)
  req <- getUpdates_ st
  json <- HTTP.sendRequest req
  Log.logDebug $ "Got response from Poll server: " <> T.lazyDecodeUtf8 json
  res <- maybe (ex json) rememberLastUpdate $ A.decode json
  extractUpdates res
  where
    ex json = throwM $ apiError $ "Unexpected Poll response: " <> T.lazyDecodeUtf8 json

sendMessageEventAnswer ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  CallbackEvent ->
  T.Text ->
  VkontakteT m A.Value
sendMessageEventAnswer cbE prompt = runMethod (SendMessageEventAnswer cbE prompt)

sendTextMessage ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Integer ->
  T.Text ->
  VkontakteT m A.Value
sendTextMessage peer_id text = runMethod (SendTextMessage peer_id text)

copyMessage ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Message ->
  VkontakteT m A.Value
copyMessage msg = runMethod (CopyMessage msg)

sendKeyboard ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Integer ->
  T.Text ->
  Keyboard ->
  VkontakteT m A.Value
sendKeyboard peer_id prompt kbd = runMethod (SendKeyboard peer_id prompt kbd)

getUpdates_ :: (Monad m) => VKState -> VkontakteT m HTTP.Request
getUpdates_ VKState {..} =
  pure . HTTP.GET $ URI.addQueryParams pollURI ["ts" URI.:=: T.unpack lastTS, "wait" URI.:=: "25"]

sendMessageEventAnswer_ :: (Monad m) => CallbackEvent -> T.Text -> VkontakteT m HTTP.Request
sendMessageEventAnswer_ CallbackEvent {..} prompt =
  fmap HTTP.GET $
    apiMethod "messages.sendMessageEventAnswer" $
      [ "event_id" URI.:=: T.unpack event_id,
        "user_id" URI.:=: show user_id,
        "peer_id" URI.:=: show peer_id,
        "event_data" URI.:=: L8.unpack . A.encode $ mkJSON prompt
      ]
  where
    mkJSON :: T.Text -> A.Value
    mkJSON p = A.object ["type" A..= ("show_snackbar" :: T.Text), "text" A..= p]

sendMessageWith ::
  (Monad m) => Integer -> T.Text -> [URI.QueryParam] -> VkontakteT m HTTP.Request
sendMessageWith peer_id text qps =
  fmap HTTP.GET $
    apiMethod "messages.send" $
      ["peer_id" URI.:=: show peer_id, "message" URI.:=: T.unpack text] <> qps

sendTextMessage_ :: (Monad m) => Integer -> T.Text -> VkontakteT m HTTP.Request
sendTextMessage_ peer_id text = sendMessageWith peer_id text mempty

copyMessage_ :: (Monad m) => Message -> VkontakteT m HTTP.Request
copyMessage_ Message {..} =
  sendMessageWith msg_peer_id msg_text $ fmap attachmentToQuery msg_attachments

sendKeyboard_ :: Monad m => Integer -> T.Text -> Keyboard -> VkontakteT m HTTP.Request
sendKeyboard_ peer_id prompt kbd =
  sendMessageWith peer_id prompt ["keyboard" URI.:=: L8.unpack $ A.encode kbd]
