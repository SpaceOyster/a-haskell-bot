{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API.Telegram
    ( module API
    , new
    , parseConfig
    , rememberLastUpdate
    , getUpdates
    , answerCallbackQuery
    , copyMessage
    , sendMessage
    , sendInlineKeyboard
    ) where

import API
import API.Telegram.Types
import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)

import Data.Aeson (Value(..), (.=), encode, object)
import Data.Function ((&))
import qualified HTTP
import System.Environment (getEnv)

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        }

new :: Config -> IO (Handle IO)
new cfg@Config {key, echoMultiplier} = do
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    http <- HTTP.new httpConfig
    lastUpdate <- newIORef echoMultiplier
    return $ Handle {http, lastUpdate}

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    return $ Config {key, defaultRepeat = 1}

withHandle :: (Handle IO -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    hAPI <- new config
    io hAPI

getLastUpdateID :: Handle m -> IO Int
getLastUpdateID = readIORef . lastUpdate

setLastUpdateID :: Handle m -> Int -> IO ()
setLastUpdateID hAPI id = lastUpdate hAPI `modifyIORef'` const id

rememberLastUpdate :: Handle m -> Update -> IO ()
rememberLastUpdate hAPI u = hAPI `setLastUpdateID` fromInteger (update_id u + 1)

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
