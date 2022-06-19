{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Vkontakte.Methods
  ( getUpdates,
    sendMessageEventAnswer,
    sendTextMessage,
    copyMessage,
    sendKeyboard,
    getUpdates_,
    sendMessageEventAnswer_,
    sendMessageWith,
    sendTextMessage_,
    copyMessage_,
    sendKeyboard_,
  )
where

import API.Vkontakte.Monad
  ( MonadVkontakte (getVKState),
    VKState (..),
    apiMethod,
    rememberLastUpdate,
  )
import API.Vkontakte.Types
  ( CallbackEvent (..),
    GroupEvent,
    Keyboard,
    Message (..),
    attachmentToQuery,
    extractUpdates,
    fromResponse,
  )
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

runMethod ::
  (MonadVkontakte m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, A.FromJSON a) =>
  Method ->
  m a
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

mkRequest :: (Monad m, MonadVkontakte m) => Method -> m HTTP.Request
mkRequest m =
  case m of
    SendMessageEventAnswer ce prompt -> sendMessageEventAnswer_ ce prompt
    SendTextMessage peer_id text -> sendTextMessage_ peer_id text
    CopyMessage msg -> copyMessage_ msg
    SendKeyboard peer_id prompt keyboard ->
      sendKeyboard_ peer_id prompt keyboard

getUpdates ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, MonadVkontakte m) =>
  m [GroupEvent]
getUpdates = do
  req <- getUpdates_
  json <- HTTP.sendRequest req
  Log.logDebug $ "Got response from Poll server: " <> T.lazyDecodeUtf8 json
  res <- maybe (ex json) rememberLastUpdate $ A.decode json
  extractUpdates res
  where
    ex json = throwM $ apiError $ "Unexpected Poll response: " <> T.lazyDecodeUtf8 json

sendMessageEventAnswer ::
  (MonadVkontakte m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  CallbackEvent ->
  T.Text ->
  m Integer
sendMessageEventAnswer cbE prompt = runMethod (SendMessageEventAnswer cbE prompt)

sendTextMessage ::
  (MonadVkontakte m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Integer ->
  T.Text ->
  m Integer
sendTextMessage peer_id text = runMethod (SendTextMessage peer_id text)

copyMessage ::
  (MonadVkontakte m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Message ->
  m Integer
copyMessage msg = runMethod (CopyMessage msg)

sendKeyboard ::
  (MonadVkontakte m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Integer ->
  T.Text ->
  Keyboard ->
  m Integer
sendKeyboard peer_id prompt kbd = runMethod (SendKeyboard peer_id prompt kbd)

getUpdates_ :: (MonadVkontakte m, Log.MonadLog m) => m HTTP.Request
getUpdates_ = do
  VKState {lastTS, wait, pollURI} <- getVKState
  Log.logDebug $ "last recieved Update TS: " <> T.tshow lastTS
  pure . HTTP.GET . URI.addQueryParams pollURI $
    [ "ts" URI.:=: T.unpack lastTS,
      "wait" URI.:=: show wait
    ]

sendMessageEventAnswer_ :: (MonadVkontakte m) => CallbackEvent -> T.Text -> m HTTP.Request
sendMessageEventAnswer_ CallbackEvent {..} prompt =
  HTTP.GET <$> apiMethod "messages.sendMessageEventAnswer" query
  where
    mkJSON :: T.Text -> A.Value
    mkJSON p = A.object ["type" A..= ("show_snackbar" :: T.Text), "text" A..= p]
    query =
      [ "event_id" URI.:=: T.unpack event_id,
        "user_id" URI.:=: show user_id,
        "peer_id" URI.:=: show peer_id,
        "event_data" URI.:=: L8.unpack . A.encode $ mkJSON prompt
      ]

sendMessageWith ::
  (MonadVkontakte m) => Integer -> T.Text -> [URI.QueryParam] -> m HTTP.Request
sendMessageWith peer_id text qps = HTTP.GET <$> apiMethod "messages.send" query
  where
    query = qps <> ["peer_id" URI.:=: show peer_id, "message" URI.:=: T.unpack text]

sendTextMessage_ :: (MonadVkontakte m) => Integer -> T.Text -> m HTTP.Request
sendTextMessage_ peer_id text = sendMessageWith peer_id text mempty

copyMessage_ :: (MonadVkontakte m) => Message -> m HTTP.Request
copyMessage_ Message {..} =
  sendMessageWith msg_peer_id msg_text $ fmap attachmentToQuery msg_attachments

sendKeyboard_ :: MonadVkontakte m => Integer -> T.Text -> Keyboard -> m HTTP.Request
sendKeyboard_ peer_id prompt kbd =
  sendMessageWith peer_id prompt ["keyboard" URI.:=: L8.unpack $ A.encode kbd]
