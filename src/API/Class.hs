{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module API.Class
    ( APIHandle(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified HTTP
import qualified Network.URI.Extended as URI

class APIHandle h
