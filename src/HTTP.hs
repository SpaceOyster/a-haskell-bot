{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HTTP
    ( Config(..)
    , Handle(..)
    , Request(..)
    , new
    , get
    , post
    ) where

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

data Config =
    Config
        {
        }

newtype Handle =
    Handle
        { manager :: H.Manager
        }

new :: Config -> IO Handle
new _cfg = do
    manager <- H.newManager tlsManagerSettings
    pure $ Handle {..}

get :: Handle -> URI.URI -> IO L8.ByteString
get handle uri = do
    req <- H.requestFromURI uri
    let req' = bakeReq req
    H.responseBody <$> H.httpLbs req' (manager handle)
  where
    bakeReq req = req {H.method = "GET"}

post :: Handle -> URI.URI -> L8.ByteString -> IO L8.ByteString
post handle uri body = do
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

data Request
    = GET URI.URI
    | POST URI.URI L8.ByteString
    deriving (Show)
