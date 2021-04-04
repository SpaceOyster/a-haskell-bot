{-# LANGUAGE OverloadedStrings #-}

module API.Telegram where

import API.Telegram.Types
import Control.Exception (handle)
import Control.Monad ((=<<))
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
    ( HttpException(..)
    , Manager(..)
    , Request(..)
    , RequestBody(..)
    , brRead
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    , responseStatus
    , withResponse
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)

makeRequest :: MonadThrow m => String -> String -> m Request
makeRequest key method =
    parseRequest $ "https://api.telegram.org/bot" ++ key ++ "/" ++ method

getUpdates :: Manager -> IO L8.ByteString
getUpdates manager = do
    apiKey <- getEnv "TG_API"
    req <- makeRequest apiKey "getUpdates"
    res <- httpLbs req manager
    return $ responseBody res

copyMessage :: Manager -> Integer -> Integer -> Integer -> IO L8.ByteString
copyMessage manager toChat fromChat mesId = do
    apiKey <- getEnv "TG_API"
    req <- makeRequest apiKey "copyMessage"
    let req' =
            req
                { method = "POST"
                , requestBody =
                      RequestBodyBS $
                      S8.pack $
                      "{\"chat_id\":" ++
                      show toChat ++
                      ",\"from_chat_id\":" ++
                      show fromChat ++ ",\"message_id\":" ++ show mesId ++ "}"
                , requestHeaders =
                      [("Content-Type", "application/json; charset=utf-8")]
                }
    res <- httpLbs req' manager
    return $ responseBody res

testMakeRequest = do
    man <- newManager tlsManagerSettings
    getUpdates man

testSendCopy toChat fromChat mesId = do
    man <- newManager tlsManagerSettings
    copyMessage man toChat fromChat mesId
