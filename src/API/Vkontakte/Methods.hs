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
    GetUpdates -> getUpdates st
    SendMessageEventAnswer ce prompt -> sendMessageEventAnswer st ce prompt
    SendTextMessage peer_id text -> sendTextMessage st peer_id text
    CopyMessage msg -> copyMessage st msg
    SendKeyboard peer_id prompt keyboard ->
      sendKeyboard st peer_id prompt keyboard

getUpdates :: (Monad m) => VKState -> VkontakteT m HTTP.Request
getUpdates VKState {..} =
  pure . HTTP.GET $ URI.addQueryParams pollURI ["ts" URI.:=: T.unpack lastTS, "wait" URI.:=: "25"]

sendMessageEventAnswer :: (Monad m) => VKState -> CallbackEvent -> T.Text -> VkontakteT m HTTP.Request
sendMessageEventAnswer st CallbackEvent {..} prompt =
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

sendTextMessage :: (Monad m) => VKState -> Integer -> T.Text -> VkontakteT m HTTP.Request
sendTextMessage st peer_id text = sendMessageWith st peer_id text mempty

copyMessage :: (Monad m) => VKState -> Message -> VkontakteT m HTTP.Request
copyMessage st Message {..} =
  sendMessageWith st peer_id text $ fmap attachmentToQuery attachments

sendKeyboard :: Monad m => VKState -> Integer -> T.Text -> Keyboard -> VkontakteT m HTTP.Request
sendKeyboard st peer_id prompt kbd =
  sendMessageWith st peer_id prompt ["keyboard" URI.:=: L8.unpack $ A.encode kbd]
