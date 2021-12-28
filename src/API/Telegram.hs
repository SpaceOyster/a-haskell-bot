{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Telegram
    ( Config(..)
    , Method(..)
    , Handle(..)
    , new
    , runMethod
    , module API.Telegram.Types
    ) where

import API.Telegram.Types
import App.Monad
import Control.Monad.Reader
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Extended ((.=), encode, object, throwDecode)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Extended as T
import qualified Exceptions as Ex
import qualified HTTP
import Handle.Class (IsHandle(..))
import qualified Logger as L
import qualified Network.URI.Extended as URI

newtype TGState =
    TGState
        { lastUpdate :: Integer
        }

data Handle =
    Handle
        { http :: HTTP.Handle
        , hLog :: L.Handle
        , baseURI :: URI.URI
        , apiState :: IORef TGState
        }

instance L.HasLog Handle where
    getLog Handle {hLog} = \p t -> L.getLog hLog p $ "API.Telegram: " <> t

getState :: (MonadIO m) => Handle -> m TGState
getState = liftIO . readIORef . apiState

modifyState :: (MonadIO m) => Handle -> (TGState -> TGState) -> m ()
modifyState hAPI morph = liftIO $ apiState hAPI `modifyIORef'` morph

setState :: (MonadIO m) => Handle -> TGState -> m ()
setState hAPI newState = liftIO . modifyState hAPI $ const newState

sendRequest ::
       (MonadIO m, MonadReader env m, Has L.Handle env)
    => Handle
    -> HTTP.Request
    -> m L8.ByteString
sendRequest hAPI req = do
    envLogDebug $ "sending request: " <> T.tshow req
    res <- liftIO $ HTTP.sendRequest (http hAPI) req
    envLogDebug $ "got response: " <> T.pack (L8.unpack res)
    pure res

newtype Config =
    Config
        { key :: String
        }

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
    maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Telegram API URL"

instance IsHandle Handle Config where
    new :: Config -> L.Handle -> IO Handle
    new cfg hLog = do
        L.logInfo hLog "Initiating Telegram API handle"
        baseURI <- makeBaseURI cfg
        let httpConfig = HTTP.Config {}
        http <- HTTP.new httpConfig
        L.logInfo hLog "HTTP handle initiated for Telegram API"
        apiState <- newIORef $ TGState 0
        pure $ Handle {..}

apiMethod :: Handle -> String -> URI.URI
apiMethod hAPI method = baseURI hAPI `URI.addPath` method

rememberLastUpdate ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Handle
    -> Response
    -> m Response
rememberLastUpdate hAPI res =
    mapM_ (setState hAPI) (newStateFromM res) >> pure res

newStateFromM :: Response -> Maybe TGState
newStateFromM (Updates us@(_x:_xs)) =
    Just . TGState . (1 +) . update_id . last $ us
newStateFromM _ = Nothing

runMethod ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Handle
    -> Method
    -> m Response
runMethod hAPI m = do
    state <- getState hAPI
    envLogDebug $ "last recieved Update id: " <> T.tshow (lastUpdate state)
    let req = mkRequest hAPI state m
    sendRequest hAPI req >>= throwDecode >>= rememberLastUpdate hAPI

data Method
    = GetUpdates
    | AnswerCallbackQuery String
    | CopyMessage Message
    | SendMessage Integer String
    | SendInlineKeyboard Integer String InlineKeyboardMarkup
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
            encode . object $
            ["offset" .= lastUpdate st, "timeout" .= (25 :: Int)]
     in HTTP.POST (apiMethod hAPI "getUpdates") json

-- API method
answerCallbackQuery :: Handle -> String -> HTTP.Request
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
sendMessage :: Handle -> Integer -> String -> HTTP.Request
sendMessage hAPI chatId msg =
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
     in HTTP.POST (apiMethod hAPI "sendMessage") json

-- API method
sendInlineKeyboard ::
       Handle -> Integer -> String -> InlineKeyboardMarkup -> HTTP.Request
sendInlineKeyboard hAPI chatId prompt keyboard =
    let json =
            encode . object $
            ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
     in HTTP.POST (apiMethod hAPI "sendMessage") json
