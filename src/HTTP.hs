{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HTTP
    ( Config(..)
    , Handle(..)
    , Request(..)
    , new
    , sendRequest
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Client as H
    ( Manager
    , Request(..)
    , RequestBody(..)
    , httpLbs
    , newManager
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
        { sendRequest :: Request -> IO L8.ByteString
        }

new :: Config -> IO Handle
new _cfg = do
    manager <- H.newManager tlsManagerSettings
    pure $
        Handle
            { sendRequest =
                  \req ->
                      case req of
                          GET uri -> get manager uri
                          POST uri body -> post manager uri body
            }

get :: H.Manager -> URI.URI -> IO L8.ByteString
get manager uri = do
    req <- H.requestFromURI uri
    let req' = bakeReq req
    H.responseBody <$> H.httpLbs req' manager
  where
    bakeReq req = req {H.method = "GET"}

post :: H.Manager -> URI.URI -> L8.ByteString -> IO L8.ByteString
post manager uri body = do
    req <- H.requestFromURI uri
    let req' = bakeReq req
    H.responseBody <$> H.httpLbs req' manager
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
