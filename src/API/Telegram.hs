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
    Types.getTextThrow,
  )
where

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
    getTextThrow,
  )
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (StateT, get, lift, put)
import Data.Aeson.Extended (encode, object, throwDecode, (.=))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Exceptions as Ex
import qualified Network.URI.Extended as URI

data TGState = TGState
  { lastUpdate :: Integer,
    apiURI :: URI.URI
  }

newtype Config = Config
  { key :: String
  }

instance Semigroup TGState where
  _a <> b = b

instance Monoid TGState where
  mempty = TGState {lastUpdate = 0, apiURI = URI.nullURI}

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Telegram API URL"

initiate :: (MonadThrow m, Log.MonadLog m) => Config -> m TGState
initiate cfg = do
  Log.logInfo "Initiating Telegram API handle"
  apiURI <- makeBaseURI cfg
  pure $ mempty {apiURI}

apiMethod :: TGState -> String -> URI.URI
apiMethod st method = apiURI st `URI.addPath` method

rememberLastUpdate ::
  (MonadThrow m, Log.MonadLog m) => Response -> StateT TGState m Response
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
  StateT TGState m Response
runMethod m = do
  state <- get
  lift $
    Log.logDebug $ "last recieved Update id: " <> T.tshow (lastUpdate state)
  let req = mkRequest state m
  res <- lift (HTTP.sendRequest req) >>= throwDecode
  lift $ Log.logDebug $ "Got response: " <> T.tshow res
  rememberLastUpdate res

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
