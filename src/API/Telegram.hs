{-# LANGUAGE OverloadedStrings #-}

module API.Telegram where

import API.Telegram.Types

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client as HTTP
    ( Manager
    , Request(..)
    , RequestBody(..)
    , Response
    , httpLbs
    , newManager
    , parseRequest_
    , responseBody
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import Utils (throwDecode)

data Config =
    Config
        { apiKey :: String
        }

data Handle m =
    Handle
        { manager :: HTTP.Manager
        , baseURL :: String
        , getRequest :: ByteString -> HTTP.Request
        , postRequest :: ByteString -> HTTP.RequestBody -> HTTP.Request
        , sendRequest :: HTTP.Request -> m (HTTP.Response L8.ByteString)
        }

new :: Config -> IO (Handle IO)
new cfg = do
    man <- newManager tlsManagerSettings
    let bURL = "https://api.telegram.org/bot" ++ apiKey cfg ++ "/"
        initReq = parseRequest_ bURL
    return $
        Handle
            { manager = man
            , baseURL = bURL
            , getRequest =
                  \apiMethod ->
                      initReq {method = "GET", path = path initReq <> apiMethod}
            , postRequest =
                  \apiMethod body ->
                      initReq
                          { method = "POST"
                          , path = path initReq <> apiMethod
                          , requestBody = body
                          , requestHeaders =
                                [ ( "Content-Type"
                                  , "application/json; charset=utf-8")
                                ]
                          }
            , sendRequest = \request -> httpLbs request man
            }

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    return $ Config {apiKey = key}

getUpdates :: (MonadThrow m) => Handle m -> m [Update]
getUpdates handle = do
    let req = getRequest handle "getUpdates"
    res <- sendRequest handle req
    let json = responseBody res
    res' <- throwDecode json
    getResult res'

echoMessage :: (Monad m) => Handle m -> Message -> m L8.ByteString
echoMessage handle msg = do
    let req =
            postRequest handle "copyMessage" .
            RequestBodyLBS . encode . copyMessage $
            msg
    res <- sendRequest handle req
    return $ responseBody res

withHandle :: (Handle IO -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    handle <- new config
    io handle

echoAll :: (MonadThrow m) => Handle m -> m [L8.ByteString]
echoAll handle = do
    updates <- getUpdates handle -- add error handling
    mapM (echoMessage handle) $ message <$> updates

reactToUpdate :: (Monad m) => Handle m -> Update -> m L8.ByteString
reactToUpdate handle update = echoMessage handle $ message update

reactToUpdates :: (Monad m) => Handle m -> [Update] -> m [L8.ByteString]
reactToUpdates handle updates = mapM (reactToUpdate handle) updates
