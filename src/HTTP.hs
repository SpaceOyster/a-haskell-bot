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
        { get :: String -> m L8.ByteString
        , post :: String -> L8.ByteString -> m L8.ByteString
        }

new :: Config -> IO (Handle IO)
new cfg = do
    man <- newManager tlsManagerSettings
    pure $ Handle {get = constructGet cfg man, post = constructPost cfg man}

constructGet :: Config -> Manager -> String -> IO L8.ByteString
constructGet cfg man apiMethod =
    let req = parseRequest_ $ baseURL cfg ++ apiMethod
        req' = req {method = "GET"}
     in responseBody <$> httpLbs req' man

constructPost ::
       Config -> Manager -> String -> L8.ByteString -> IO L8.ByteString
constructPost cfg man apiMethod body =
    let req = parseRequest_ $ baseURL cfg ++ apiMethod
        req' =
            req
                { method = "POST"
                , requestBody = RequestBodyLBS body
                , requestHeaders =
                      [("Content-Type", "application/json; charset=utf-8")]
                }
     in responseBody <$> httpLbs req' man
