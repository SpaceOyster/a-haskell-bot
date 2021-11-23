{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module API.Class
    ( Request(..)
    , APIHandle(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified HTTP
import qualified Network.URI.Extended as URI

data Request
    = GET URI.URI
    | POST URI.URI L8.ByteString
    deriving (Show)

class APIHandle h
