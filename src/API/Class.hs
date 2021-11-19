{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module API.Class
    ( Request(..)
    , credsToRequest
    , PollCreds(..)
    , APIHandle(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified HTTP
import qualified Network.URI.Extended as URI

data PollCreds =
    PollCreds
        { pollURI :: URI.URI
        , queryParams :: URI.QueryParams
        , body :: L8.ByteString
        }
    deriving (Show)

credsToURI :: PollCreds -> URI.URI
credsToURI PollCreds {pollURI, queryParams} =
    pollURI `URI.addQueryParams` queryParams

credsToRequest :: PollCreds -> Request
credsToRequest p@PollCreds {body} =
    if L8.null body
        then GET uri
        else POST uri body
  where
    uri = credsToURI p

instance Semigroup PollCreds where
    a <> b = b

instance Monoid PollCreds where
    mempty =
        PollCreds {pollURI = URI.nullURI, queryParams = mempty, body = L8.empty}

data Request
    = GET URI.URI
    | POST URI.URI L8.ByteString
    deriving (Show)

class APIHandle h
