{-# LANGUAGE OverloadedStrings #-}

module HTTP where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client
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

data Config =
    Config
        { baseURL :: String
        }

data Handle m =
    Handle
        { getRequest :: ByteString -> Request
        , postRequest :: ByteString -> RequestBody -> Request
        , sendRequest :: Request -> m (Response L8.ByteString)
        }

parseConfig :: IO Config
parseConfig = undefined

new :: Config -> IO (Handle IO)
new cfg = do
    man <- newManager tlsManagerSettings
    let initReq = parseRequest_ $ baseURL cfg
    return $
        Handle
            { getRequest =
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
