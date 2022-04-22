{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Telegram.Methods
  ( answerCallbackQuery,
    copyMessage,
    getUpdates,
    sendInlineKeyboard,
    sendMessage,
  )
where

import API.Telegram.Monad
import API.Telegram.Types as Types
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (MonadState (..), get)
import Control.Monad.Trans (MonadTrans (..), lift)
import Data.Aeson as A (FromJSON, Value (..), decode, encode, object, (.=))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

data Method
  = GetUpdates
  | AnswerCallbackQuery T.Text
  | CopyMessage Message
  | SendMessage Integer T.Text
  | SendInlineKeyboard Integer T.Text InlineKeyboardMarkup
  deriving (Show)

getUpdates ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  TelegramT m [Update]
getUpdates = runMethod GetUpdates >>= rememberLastUpdate

answerCallbackQuery ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  T.Text ->
  TelegramT m Bool
answerCallbackQuery c = runMethod (AnswerCallbackQuery c)

copyMessage ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Message ->
  TelegramT m Value
copyMessage msg = runMethod (CopyMessage msg)

sendMessage ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Integer ->
  T.Text ->
  TelegramT m Message
sendMessage chatId text = runMethod (SendMessage chatId text)

sendInlineKeyboard ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Integer ->
  T.Text ->
  InlineKeyboardMarkup ->
  TelegramT m Message
sendInlineKeyboard chatId prompt keyboard =
  runMethod (SendInlineKeyboard chatId prompt keyboard)

apiMethod :: Monad m => String -> TelegramT m URI.URI
apiMethod method = do
  st <- get
  pure $ apiURI st `URI.addPath` method

runMethod ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, A.FromJSON a) =>
  Method ->
  TelegramT m a
runMethod m = do
  req <- mkRequest m
  json <- lift $ HTTP.sendRequest req
  Log.logDebug $ "Got response: " <> T.lazyDecodeUtf8 json
  maybe (ex json) fromResponse $ A.decode json
  where
    ex json = throwM . apiError $ "Unexpected response: " <> T.lazyDecodeUtf8 json

mkRequest :: Log.MonadLog m => Method -> TelegramT m HTTP.Request
mkRequest m =
  case m of
    GetUpdates -> getUpdates_
    AnswerCallbackQuery cqid -> answerCallbackQuery_ cqid
    CopyMessage msg -> copyMessage_ msg
    SendMessage chatId msg -> sendMessage_ chatId msg
    SendInlineKeyboard chatId prompt keyboard ->
      sendInlineKeyboard_ chatId prompt keyboard

getUpdates_ :: Log.MonadLog m => TelegramT m HTTP.Request
getUpdates_ = do
  st <- get
  Log.logDebug $ "last recieved Update id: " <> T.tshow (lastUpdate st)
  let json =
        encode . object $ ["offset" .= lastUpdate st, "timeout" .= (25 :: Int)]
  uri <- apiMethod "getUpdates"
  pure $ HTTP.POST uri json

answerCallbackQuery_ :: Monad m => T.Text -> TelegramT m HTTP.Request
answerCallbackQuery_ cqid = do
  let json = encode . object $ ["callback_query_id" .= cqid]
  uri <- apiMethod "answerCallbackQuery"
  pure $ HTTP.POST uri json

copyMessage_ :: Monad m => Message -> TelegramT m HTTP.Request
copyMessage_ Message {message_id, chat} = do
  let json =
        encode . object $
          [ "chat_id" .= chat_id (chat :: Chat),
            "from_chat_id" .= chat_id (chat :: Chat),
            "message_id" .= message_id
          ]
  uri <- apiMethod "copyMessage"
  pure $ HTTP.POST uri json

sendMessage_ :: Monad m => Integer -> T.Text -> TelegramT m HTTP.Request
sendMessage_ chatId msg = do
  let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
  uri <- apiMethod "sendMessage"
  pure $ HTTP.POST uri json

sendInlineKeyboard_ ::
  Monad m => Integer -> T.Text -> InlineKeyboardMarkup -> TelegramT m HTTP.Request
sendInlineKeyboard_ chatId prompt keyboard = do
  let json =
        encode . object $
          ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
  uri <- apiMethod "sendMessage"
  pure $ HTTP.POST uri json
