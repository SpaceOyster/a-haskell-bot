{-# LANGUAGE OverloadedStrings #-}

module API.Telegram where

import API.Telegram.Types

-- import Control.Exception (handle)
-- import Control.Monad ((=<<))
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode)

-- import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromJust, fromMaybe)
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
import Utils (throwDecode)

data Config =
    Config
        { apiKey :: String
        }

data Handle =
    Handle
        { manager :: Manager
        , baseURL :: String
        }

new :: Config -> IO Handle
new cfg = do
    man <- newManager tlsManagerSettings
    let bURL = "https://api.telegram.org/bot" ++ (apiKey cfg) ++ "/"
    return $ Handle {manager = man, baseURL = bURL}

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    return $ Config {apiKey = key}

makeRequest :: MonadThrow m => Handle -> String -> m Request
makeRequest handle method = parseRequest $ baseURL handle ++ method

getUpdates :: Handle -> IO [Update]
getUpdates handle = do
    req <- makeRequest handle "getUpdates"
    res <- httpLbs req $ manager handle
    let json = responseBody res
    res' <- throwDecode json
    getResult res'

echoMessage :: Handle -> Message -> IO L8.ByteString
echoMessage handle msg = do
    req <- makeRequest handle "copyMessage"
    let req' =
            req
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode $ copyMessage msg
                , requestHeaders =
                      [("Content-Type", "application/json; charset=utf-8")]
                }
    res <- httpLbs req' $ manager handle
    return $ responseBody res

withHandle :: (Handle -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    handle <- new config
    io handle

echoAll :: Handle -> IO [L8.ByteString]
echoAll handle = do
    updates <- getUpdates handle -- add error handling
    mapM (echoMessage handle) $ message <$> updates

reactToUpdate :: Handle -> Update -> IO L8.ByteString
reactToUpdate handle update = echoMessage handle $ message update

reactToUpdates :: Handle -> [Update] -> IO [L8.ByteString]
reactToUpdates handle updates = mapM (reactToUpdate handle) updates

doBotThing :: Handle -> IO [L8.ByteString]
doBotThing handle = getUpdates handle >>= reactToUpdates handle

botLoop :: Handle -> IO [L8.ByteString]
botLoop handle = undefined

testGetUpdates = withHandle getUpdates

testSendCopy msg = withHandle (`echoMessage` msg)
