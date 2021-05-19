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
import Data.IORef (modifyIORef, newIORef, readIORef)

import Data.Aeson (Value(..), (.=), encode, object)
import Data.Function ((&))
import qualified HTTP
import System.Environment (getEnv)

data Config =
    Config
        { key :: String
        , defaultRepeat :: Integer
        }

new :: Config -> IO (Handle IO)
new cfg@Config {key, defaultRepeat} = do
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    http <- HTTP.new httpConfig
    lastUpdate <- newIORef defaultRepeat
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

getLastUpdateID :: Handle m -> IO Integer
getLastUpdateID = readIORef . lastUpdate

setLastUpdateID :: Handle m -> Integer -> IO ()
setLastUpdateID hAPI id = lastUpdate hAPI `modifyIORef` const id

rememberLastUpdate :: Handle m -> Update -> IO ()
rememberLastUpdate hAPI u = hAPI `setLastUpdateID` (update_id u + 1)

getUpdates :: Handle IO -> IO API.Request
getUpdates hAPI =
    bracket (getLastUpdateID hAPI) (const $ return ()) $ \id ->
        let json = encode . object $ ["offset" .= id]
         in return $ POST "getUpdates" json

data QueryData
    = QDRepeat Int
    | QDOther String
    deriving (Show)

qualifyQuery :: String -> QueryData
qualifyQuery qstring =
    case qtype of
        "repeat" -> QDRepeat $ read (tail qdata)
        _ -> QDOther qstring
  where
    (qtype, qdata) = break (== '_') qstring

answerCallbackQuery :: (Monad m) => Handle m -> String -> m API.Request
answerCallbackQuery hAPI id = do
    let json = encode . object $ ["callback_query_id" .= id]
    return $ POST "answerCallbackQuery" json

copyMessage :: (Monad m) => Message -> m API.Request
copyMessage msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    return $ POST "copyMessage" json

sendMessage :: (Monad m) => Integer -> String -> m API.Request
sendMessage chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    return $ POST "sendMessage" json

sendInlineKeyboard ::
       (Monad m) => Integer -> String -> InlineKeyboardMarkup -> m API.Request
sendInlineKeyboard chatId prompt keyboard = do
    let json =
            encode . object $
            ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
    return $ POST "sendMessage" json
