{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HTTP where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Client as H
    ( Manager
    , Request(..)
    , RequestBody(..)
    , Response
    , httpLbs
    , newManager
    , parseRequest_
    , requestFromURI
    , responseBody
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.URI.Extended as URI
import System.Environment (getEnv)

data Config =
    Config
        { baseURI :: URI.URI
        }

data Handle =
    Handle
        { manager :: H.Manager
        , baseURI :: URI.URI
        }

new :: Config -> IO Handle
new Config {..} = do
    manager <- H.newManager tlsManagerSettings
    pure $ Handle {..}

get :: Handle -> String -> IO L8.ByteString
get handle url =
    let req = H.parseRequest_ url
        req' = req {H.method = "GET"}
     in H.responseBody <$> H.httpLbs req' (manager handle)

post :: Handle -> String -> L8.ByteString -> IO L8.ByteString
post handle url body =
    let req = H.parseRequest_ url
        req' =
            req
                { H.method = "POST"
                , H.requestBody = H.RequestBodyLBS body
                , H.requestHeaders =
                      [("Content-Type", "application/json; charset=utf-8")]
                }
     in H.responseBody <$> H.httpLbs req' (manager handle)

get' :: Handle -> URI.URI -> IO L8.ByteString
get' handle uri = do
    req <- H.requestFromURI uri
    let req' = bakeReq req
    H.responseBody <$> H.httpLbs req' (manager handle)
  where
    bakeReq req = req {H.method = "GET"}

post' :: Handle -> URI.URI -> L8.ByteString -> IO L8.ByteString
post' handle uri body = do
    req <- H.requestFromURI uri
    let req' = bakeReq req
    H.responseBody <$> H.httpLbs req' (manager handle)
  where
    bakeReq req =
        req
            { H.method = "POST"
            , H.requestBody = H.RequestBodyLBS body
            , H.requestHeaders =
                  [("Content-Type", "application/json; charset=utf-8")]
            }
