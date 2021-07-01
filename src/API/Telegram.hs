{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

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

import Data.Aeson (Value(..), (.=), encode, object)
import Data.Function ((&))
import qualified HTTP
import qualified Logger
import System.Environment (getEnv)

data Config =
    Config
        { key :: String
        }

new :: Config -> Logger.Handle -> IO (Handle IO)
new cfg@Config {key} hLog = do
    Logger.info' hLog "Initiating Telegram API handle"
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    http <- HTTP.new httpConfig
    Logger.info' hLog "HTTP handle initiated for Telegram API"
    lastUpdate <- newIORef 0
    return $ Handle {http, hLog, lastUpdate}

withHandle :: Config -> Logger.Handle -> (Handle IO -> IO a) -> IO a
withHandle config hLog io = do
    hAPI <- new config hLog
    io hAPI

getLastUpdateID :: Handle m -> IO Integer
getLastUpdateID = readIORef . lastUpdate

setLastUpdateID :: Handle m -> Integer -> IO ()
setLastUpdateID hAPI id = lastUpdate hAPI `modifyIORef'` const id

rememberLastUpdate :: Handle m -> Update -> IO ()
rememberLastUpdate hAPI u = hAPI `setLastUpdateID` (update_id u + 1)

-- API method
getUpdates :: Handle IO -> IO API.Request
getUpdates hAPI =
    bracket (getLastUpdateID hAPI) (const $ return ()) $ \id ->
        let json = encode . object $ ["offset" .= id]
         in return $ POST "getUpdates" json

-- API method
answerCallbackQuery :: (Monad m) => Handle m -> String -> m API.Request
answerCallbackQuery hAPI id = do
    let json = encode . object $ ["callback_query_id" .= id]
    return $ POST "answerCallbackQuery" json

-- API method
copyMessage :: (Monad m) => Message -> m API.Request
copyMessage msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    return $ POST "copyMessage" json

-- API method
sendMessage :: (Monad m) => Integer -> String -> m API.Request
sendMessage chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    return $ POST "sendMessage" json

-- API method
sendInlineKeyboard ::
       (Monad m) => Integer -> String -> InlineKeyboardMarkup -> m API.Request
sendInlineKeyboard chatId prompt keyboard = do
    let json =
            encode . object $
            ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
    return $ POST "sendMessage" json
