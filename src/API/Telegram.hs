{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module API.Telegram
  ( Config(..)
  , Method(..)
  , Handle(..)
  , new
  , runMethod
  , Types.CallbackQuery(..)
  , Types.Error(..)
  , Types.Response(..)
  , Types.InlineKeyboardButton(..)
  , Types.InlineKeyboardMarkup(..)
  , Types.Chat(..)
  , Types.User(..)
  , Types.Message(..)
  , Types.Update(..)
  , Types.extractUpdates
  , Types.getAuthorThrow
  , Types.getQDataThrow
  , Types.getTextThrow
  ) where

import API.Telegram.Types as Types
  ( CallbackQuery(..)
  , Chat(..)
  , Error(..)
  , InlineKeyboardButton(..)
  , InlineKeyboardMarkup(..)
  , Message(..)
  , Response(..)
  , Update(..)
  , User(..)
  , extractUpdates
  , getAuthorThrow
  , getQDataThrow
  , getTextThrow
  )
import App.Env (grab)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Extended ((.=), encode, object, throwDecode)
import qualified Data.Text.Extended as T
import qualified Effects.Log as Log
import qualified Exceptions as Ex
import qualified HTTP
import qualified Network.URI.Extended as URI

newtype TGState =
  TGState
    { lastUpdate :: Integer
    }

data Handle =
  Handle
    { baseURI :: URI.URI
    , apiState :: IORef TGState
    }

getState :: (MonadIO m) => Handle -> m TGState
getState = liftIO . readIORef . apiState

modifyState :: (MonadIO m) => Handle -> (TGState -> TGState) -> m ()
modifyState hAPI morph = liftIO $ apiState hAPI `modifyIORef'` morph

setState :: (MonadIO m) => Handle -> TGState -> m ()
setState hAPI newState = liftIO . modifyState hAPI $ const newState

newtype Config =
  Config
    { key :: String
    }

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Telegram API URL"

new :: (MonadIO m, MonadThrow m, Log.MonadLog m) => Config -> m Handle
new cfg = do
  Log.logInfo "Initiating Telegram API handle"
  baseURI <- makeBaseURI cfg
  apiState <- liftIO $ newIORef $ TGState 0
  pure $ Handle {..}

apiMethod :: Handle -> String -> URI.URI
apiMethod hAPI method = baseURI hAPI `URI.addPath` method

rememberLastUpdate ::
     (MonadIO m, MonadThrow m, MonadReader env m, Log.MonadLog m)
  => Handle
  -> Response
  -> m Response
rememberLastUpdate hAPI res =
  mapM_ (setState hAPI) (newStateFromM res) >> pure res

newStateFromM :: Response -> Maybe TGState
newStateFromM (UpdatesResponse us@(_x:_xs)) =
  Just . TGState . (1 +) . update_id . last $ us
newStateFromM _ = Nothing

runMethod ::
     ( MonadIO m
     , MonadThrow m
     , MonadReader env m
     , Has HTTP.Handle env
     , Log.MonadLog m
     )
  => Handle
  -> Method
  -> m Response
runMethod hAPI m = do
  state <- getState hAPI
  Log.logDebug $ "last recieved Update id: " <> T.tshow (lastUpdate state)
  let req = mkRequest hAPI state m
  hHTTP <- grab @HTTP.Handle
  liftIO (HTTP.sendRequest hHTTP req) >>= throwDecode >>=
    rememberLastUpdate hAPI

data Method
  = GetUpdates
  | AnswerCallbackQuery T.Text
  | CopyMessage Message
  | SendMessage Integer T.Text
  | SendInlineKeyboard Integer T.Text InlineKeyboardMarkup
  deriving (Show)

mkRequest :: Handle -> TGState -> Method -> HTTP.Request
mkRequest hAPI s m =
  case m of
    GetUpdates -> getUpdates hAPI s
    AnswerCallbackQuery cqid -> answerCallbackQuery hAPI cqid
    CopyMessage msg -> copyMessage hAPI msg
    SendMessage chatId msg -> sendMessage hAPI chatId msg
    SendInlineKeyboard chatId prompt keyboard ->
      sendInlineKeyboard hAPI chatId prompt keyboard

-- API method
getUpdates :: Handle -> TGState -> HTTP.Request
getUpdates hAPI st =
  let json =
        encode . object $ ["offset" .= lastUpdate st, "timeout" .= (25 :: Int)]
   in HTTP.POST (apiMethod hAPI "getUpdates") json

-- API method
answerCallbackQuery :: Handle -> T.Text -> HTTP.Request
answerCallbackQuery hAPI cqid =
  let json = encode . object $ ["callback_query_id" .= cqid]
   in HTTP.POST (apiMethod hAPI "answerCallbackQuery") json

-- API method
copyMessage :: Handle -> Message -> HTTP.Request
copyMessage hAPI Message {message_id, chat} =
  let json =
        encode . object $
        [ "chat_id" .= chat_id (chat :: Chat)
        , "from_chat_id" .= chat_id (chat :: Chat)
        , "message_id" .= message_id
        ]
   in HTTP.POST (apiMethod hAPI "copyMessage") json

-- API method
sendMessage :: Handle -> Integer -> T.Text -> HTTP.Request
sendMessage hAPI chatId msg =
  let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
   in HTTP.POST (apiMethod hAPI "sendMessage") json

-- API method
sendInlineKeyboard ::
     Handle -> Integer -> T.Text -> InlineKeyboardMarkup -> HTTP.Request
sendInlineKeyboard hAPI chatId prompt keyboard =
  let json =
        encode . object $
        ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
   in HTTP.POST (apiMethod hAPI "sendMessage") json
