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
        {
        }

data Handle =
    Handle
        { manager :: Manager
        }

new :: Config -> IO Handle
new cfg = do
    man <- newManager tlsManagerSettings
    pure $ Handle {manager = man}

get :: Handle -> String -> IO L8.ByteString
get handle url =
    let req = parseRequest_ url
        req' = req {method = "GET"}
     in responseBody <$> httpLbs req' (manager handle)

post :: Handle -> String -> L8.ByteString -> IO L8.ByteString
post handle url body =
    let req = parseRequest_ url
        req' =
            req
                { method = "POST"
                , requestBody = RequestBodyLBS body
                , requestHeaders =
                      [("Content-Type", "application/json; charset=utf-8")]
                }
     in responseBody <$> httpLbs req' (manager handle)
