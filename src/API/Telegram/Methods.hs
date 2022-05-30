{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Telegram.Methods
  ( answerCallbackQuery,
    copyMessage,
    getUpdates,
    sendInlineKeyboard,
    sendMessage,
    answerCallbackQuery_,
    copyMessage_,
    getUpdates_,
    sendInlineKeyboard_,
    sendMessage_,
    MessageID (..),
  )
where

import API.Telegram.Monad
  ( MonadTelegram (getTGState),
    TGState (lastUpdate, timeout),
    apiMethod,
    rememberLastUpdate,
  )
import API.Telegram.Types as Types
  ( Chat (chat_id),
    InlineKeyboardMarkup,
    Message (Message, chat, message_id),
    Update,
    fromResponse,
  )
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson as A (FromJSON, decode, encode, object, (.=))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import GHC.Generics (Generic)

data Method
  = GetUpdates
  | AnswerCallbackQuery T.Text
  | CopyMessage Message
  | SendMessage Integer T.Text
  | SendInlineKeyboard Integer T.Text InlineKeyboardMarkup
  deriving (Show)

getUpdates ::
  (MonadTelegram m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  m [Update]
getUpdates = runMethod GetUpdates >>= rememberLastUpdate

answerCallbackQuery ::
  (MonadTelegram m, MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  T.Text ->
  m Bool
answerCallbackQuery c = runMethod (AnswerCallbackQuery c)

newtype MessageID = MessageID {message_id :: Integer}
  deriving (Eq, Show, Generic, FromJSON)

copyMessage ::
  (MonadTelegram m, MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Message ->
  m MessageID
copyMessage msg = runMethod (CopyMessage msg)

sendMessage ::
  (MonadTelegram m, MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m) =>
  Integer ->
  T.Text ->
  m Message
sendMessage chatId text = runMethod (SendMessage chatId text)

sendInlineKeyboard ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m, MonadTelegram m) =>
  Integer ->
  T.Text ->
  InlineKeyboardMarkup ->
  m Message
sendInlineKeyboard chatId prompt keyboard =
  runMethod (SendInlineKeyboard chatId prompt keyboard)

runMethod ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, A.FromJSON a, MonadTelegram m) =>
  Method ->
  m a
runMethod m = do
  req <- mkRequest m
  json <- HTTP.sendRequest req
  Log.logDebug $ "Got response: " <> T.lazyDecodeUtf8 json
  maybe (ex json) fromResponse $ A.decode json
  where
    ex json = throwM . apiError $ "Unexpected response: " <> T.lazyDecodeUtf8 json

mkRequest :: (MonadTelegram m, Log.MonadLog m) => Method -> m HTTP.Request
mkRequest m =
  case m of
    GetUpdates -> getUpdates_
    AnswerCallbackQuery cqid -> answerCallbackQuery_ cqid
    CopyMessage msg -> copyMessage_ msg
    SendMessage chatId msg -> sendMessage_ chatId msg
    SendInlineKeyboard chatId prompt keyboard ->
      sendInlineKeyboard_ chatId prompt keyboard

getUpdates_ :: (MonadTelegram m, Log.MonadLog m) => m HTTP.Request
getUpdates_ = do
  st <- getTGState
  Log.logDebug $ "last recieved Update id: " <> T.tshow (lastUpdate st)
  let json =
        encode . object $ ["offset" .= lastUpdate st, "timeout" .= timeout st]
  uri <- apiMethod "getUpdates"
  pure $ HTTP.POST uri json

answerCallbackQuery_ :: MonadTelegram m => T.Text -> m HTTP.Request
answerCallbackQuery_ cqid = do
  let json = encode . object $ ["callback_query_id" .= cqid]
  uri <- apiMethod "answerCallbackQuery"
  pure $ HTTP.POST uri json

copyMessage_ :: MonadTelegram m => Message -> m HTTP.Request
copyMessage_ Message {message_id, chat} = do
  let chatId = chat_id (chat :: Chat)
      json =
        encode . object $
          [ "chat_id" .= chatId,
            "from_chat_id" .= chatId,
            "message_id" .= message_id
          ]
  uri <- apiMethod "copyMessage"
  pure $ HTTP.POST uri json

sendMessage_ :: MonadTelegram m => Integer -> T.Text -> m HTTP.Request
sendMessage_ chatId msg = do
  let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
  uri <- apiMethod "sendMessage"
  pure $ HTTP.POST uri json

sendInlineKeyboard_ ::
  MonadTelegram m => Integer -> T.Text -> InlineKeyboardMarkup -> m HTTP.Request
sendInlineKeyboard_ chatId prompt keyboard = do
  let json =
        encode . object $
          ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
  uri <- apiMethod "sendMessage"
  pure $ HTTP.POST uri json
