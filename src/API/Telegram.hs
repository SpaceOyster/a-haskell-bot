{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module API.Telegram
  ( Config (..),
    Method (..),
    TGState (..),
    initiate,
    runMethod,
    TelegramT (..),
    Types.CallbackQuery (..),
    Types.Error (..),
    Types.Response (..),
    Types.InlineKeyboardButton (..),
    Types.InlineKeyboardMarkup (..),
    Types.Chat (..),
    Types.User (..),
    Types.Message (..),
    Types.Update (..),
    Types.extractUpdates,
    Types.getAuthorThrow,
    Types.getQDataThrow,
    module API.Telegram.Monad,
  )
where

import API.Telegram.Monad
import API.Telegram.Types as Types
  ( CallbackQuery (..),
    Chat (..),
    Error (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    Message (..),
    Response (..),
    Update (..),
    User (..),
    extractUpdates,
    getAuthorThrow,
    getQDataThrow,
  )
import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (MonadState (..), get, put)
import Control.Monad.Trans (MonadTrans (..), lift)
import Data.Aeson (decode, encode, object, (.=))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ apiError "Unable to parse Telegram API URL"

initiate :: (MonadThrow m, Log.MonadLog m) => Config -> m TGState
initiate cfg = do
  Log.logInfo "Initiating Telegram API handle"
  apiURI <- makeBaseURI cfg
  pure $ mempty {apiURI}

apiMethod :: TGState -> String -> URI.URI
apiMethod st method = apiURI st `URI.addPath` method

rememberLastUpdate ::
  (MonadThrow m, Log.MonadLog m) => Response -> TelegramT m Response
rememberLastUpdate res = do
  st <- get
  mapM_ put (newStateFromM res st) >> pure res

newStateFromM :: Response -> TGState -> Maybe TGState
newStateFromM (UpdatesResponse us@(_x : _xs)) st =
  let uid = (1 +) . update_id . last $ us
   in Just $ st {lastUpdate = uid}
newStateFromM _ _ = Nothing

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
    GetUpdates -> getUpdates st
    AnswerCallbackQuery cqid -> answerCallbackQuery st cqid
    CopyMessage msg -> copyMessage st msg
    SendMessage chatId msg -> sendMessage st chatId msg
    SendInlineKeyboard chatId prompt keyboard ->
      sendInlineKeyboard st chatId prompt keyboard

-- API method
getUpdates :: TGState -> HTTP.Request
getUpdates st =
  let json =
        encode . object $ ["offset" .= lastUpdate st, "timeout" .= (25 :: Int)]
   in HTTP.POST (apiMethod st "getUpdates") json

-- API method
answerCallbackQuery :: TGState -> T.Text -> HTTP.Request
answerCallbackQuery st cqid =
  let json = encode . object $ ["callback_query_id" .= cqid]
   in HTTP.POST (apiMethod st "answerCallbackQuery") json

-- API method
copyMessage :: TGState -> Message -> HTTP.Request
copyMessage st Message {message_id, chat} =
  let json =
        encode . object $
          [ "chat_id" .= chat_id (chat :: Chat),
            "from_chat_id" .= chat_id (chat :: Chat),
            "message_id" .= message_id
          ]
   in HTTP.POST (apiMethod st "copyMessage") json

-- API method
sendMessage :: TGState -> Integer -> T.Text -> HTTP.Request
sendMessage st chatId msg =
  let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
   in HTTP.POST (apiMethod st "sendMessage") json

-- API method
sendInlineKeyboard ::
  TGState -> Integer -> T.Text -> InlineKeyboardMarkup -> HTTP.Request
sendInlineKeyboard st chatId prompt keyboard =
  let json =
        encode . object $
          ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
   in HTTP.POST (apiMethod st "sendMessage") json
