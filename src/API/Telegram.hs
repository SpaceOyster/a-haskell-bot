{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module API.Telegram
    ( module API
    , new
    , rememberLastUpdate
    , getUpdates
    , answerCallbackQuery
    , copyMessage
    , sendMessage
    , sendInlineKeyboard
    , Config(..)
    ) where

import API
import API.Telegram.Types
import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (Value(..), (.=), encode, object)
import Data.Function ((&))
import qualified Exceptions as Ex
import qualified HTTP
import qualified Logger
import qualified Network.URI.Extended as URI
import System.Environment (getEnv)

data Config =
    Config
        { key :: String
        }

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
    maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Telegram API URL"

new :: Config -> Logger.Handle -> IO Handle
new cfg@Config {key} hLog = do
    Logger.info' hLog "Initiating Telegram API handle"
    baseURI <- makeBaseURI cfg
    let baseURL = "https://api.telegram.org/bot" <> key <> "/"
        httpConfig = HTTP.Config {baseURI}
    http <- HTTP.new httpConfig
    Logger.info' hLog "HTTP handle initiated for Telegram API"
    lastUpdate <- newIORef 0
    pure $ Handle {http, hLog, lastUpdate, baseURL}

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config hLog io = do
    hAPI <- new config hLog
    io hAPI

apiMethod :: Handle -> String -> URI.URI
apiMethod hAPI method = baseURI hAPI `URI.addPath` method

getLastUpdateID :: Handle -> IO Integer
getLastUpdateID = readIORef . lastUpdate

setLastUpdateID :: Handle -> Integer -> IO ()
setLastUpdateID hAPI id = lastUpdate hAPI `modifyIORef'` const id

rememberLastUpdate :: Handle -> Update -> IO ()
rememberLastUpdate hAPI u = hAPI `setLastUpdateID` (update_id u + 1)

-- API method
getUpdates :: Handle -> IO API.Request
getUpdates hAPI@Handle {hLog} =
    bracket (getLastUpdateID hAPI) (const $ pure ()) $ \id -> do
        Logger.debug' hLog $ "Telegram: last recieved Update id: " <> show id
        let json = encode . object $ ["offset" .= id]
        pure $ POST (apiMethod hAPI "getUpdates") json

-- API method
answerCallbackQuery :: (Monad m) => Handle -> String -> m API.Request
answerCallbackQuery hAPI id = do
    let json = encode . object $ ["callback_query_id" .= id]
    pure $ POST (apiMethod hAPI "answerCallbackQuery") json

-- API method
copyMessage :: (Monad m) => Handle -> Message -> m API.Request
copyMessage hAPI msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    pure $ POST (apiMethod hAPI "copyMessage") json

-- API method
sendMessage :: (Monad m) => Handle -> Integer -> String -> m API.Request
sendMessage hAPI chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    pure $ POST (apiMethod hAPI "sendMessage") json

-- API method
sendInlineKeyboard ::
       (Monad m)
    => Handle
    -> Integer
    -> String
    -> InlineKeyboardMarkup
    -> m API.Request
sendInlineKeyboard hAPI chatId prompt keyboard = do
    let json =
            encode . object $
            ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
    pure $ POST (apiMethod hAPI "sendMessage") json
