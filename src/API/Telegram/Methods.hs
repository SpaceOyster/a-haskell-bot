{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Telegram.Methods where

import API.Telegram.Monad
import API.Telegram.Types as Types
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (MonadState (..), get)
import Control.Monad.Trans (MonadTrans (..), lift)
import Data.Aeson (decode, encode, object, (.=))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

apiMethod :: TGState -> String -> URI.URI
apiMethod st method = apiURI st `URI.addPath` method

runMethod ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Method ->
  TelegramT m Response
runMethod m = do
  st <- get
  lift $ Log.logDebug $ "last recieved Update id: " <> T.tshow (lastUpdate st)
  let req = mkRequest st m
  json <- lift $ HTTP.sendRequest req
  lift $ Log.logDebug $ "Got response: " <> T.lazyDecodeUtf8 json
  maybe (ex json) rememberLastUpdate $ decode json
  where
    ex json = throwM $ apiError $ "Unexpected response: " <> T.lazyDecodeUtf8 json

data Method
  = GetUpdates
  | AnswerCallbackQuery T.Text
  | CopyMessage Message
  | SendMessage Integer T.Text
  | SendInlineKeyboard Integer T.Text InlineKeyboardMarkup
  deriving (Show)

mkRequest :: TGState -> Method -> HTTP.Request
mkRequest st m =
  case m of
    GetUpdates -> getUpdates_ st
    AnswerCallbackQuery cqid -> answerCallbackQuery_ st cqid
    CopyMessage msg -> copyMessage_ st msg
    SendMessage chatId msg -> sendMessage_ st chatId msg
    SendInlineKeyboard chatId prompt keyboard ->
      sendInlineKeyboard_ st chatId prompt keyboard

getUpdates ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  TelegramT m [Update]
getUpdates = runMethod GetUpdates >>= extractUpdates

answerCallbackQuery ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  T.Text ->
  TelegramT m Response
answerCallbackQuery = runMethod . AnswerCallbackQuery

copyMessage ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Message ->
  TelegramT m Response
copyMessage = runMethod . CopyMessage

sendMessage ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Integer ->
  T.Text ->
  TelegramT m Response
sendMessage chatId = runMethod . SendMessage chatId

sendInlineKeyboard ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Integer ->
  T.Text ->
  InlineKeyboardMarkup ->
  TelegramT m Response
sendInlineKeyboard chatId prompt keyboard = runMethod $ SendInlineKeyboard chatId prompt keyboard

getUpdates_ :: TGState -> HTTP.Request
getUpdates_ st =
  let json =
        encode . object $ ["offset" .= lastUpdate st, "timeout" .= (25 :: Int)]
   in HTTP.POST (apiMethod st "getUpdates") json

answerCallbackQuery_ :: TGState -> T.Text -> HTTP.Request
answerCallbackQuery_ st cqid =
  let json = encode . object $ ["callback_query_id" .= cqid]
   in HTTP.POST (apiMethod st "answerCallbackQuery") json

copyMessage_ :: TGState -> Message -> HTTP.Request
copyMessage_ st Message {message_id, chat} =
  let json =
        encode . object $
          [ "chat_id" .= chat_id (chat :: Chat),
            "from_chat_id" .= chat_id (chat :: Chat),
            "message_id" .= message_id
          ]
   in HTTP.POST (apiMethod st "copyMessage") json

sendMessage_ :: TGState -> Integer -> T.Text -> HTTP.Request
sendMessage_ st chatId msg =
  let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
   in HTTP.POST (apiMethod st "sendMessage") json

sendInlineKeyboard_ ::
  TGState -> Integer -> T.Text -> InlineKeyboardMarkup -> HTTP.Request
sendInlineKeyboard_ st chatId prompt keyboard =
  let json =
        encode . object $
          ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
   in HTTP.POST (apiMethod st "sendMessage") json
