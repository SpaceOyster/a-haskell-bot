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
  let req = mkRequest st m
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
extractUpdates (PollError c) = throwM $ apiError $ "Vkontakte Poll Error: " <> T.tshow c
extractUpdates _ = throwM $ apiError "Expexted poll response"

sendKeyboard :: VKState -> Integer -> T.Text -> Keyboard -> HTTP.Request
sendKeyboard st peer_id prompt kbd =
  sendMessageWith st peer_id prompt ["keyboard" URI.:=: L8.unpack $ A.encode kbd]
