{-# LANGUAGE DuplicateRecordFields #-}

module API
    ( Handle(..)
    , Request(..)
    , sendRequest
    , baseURI
    ) where

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef
import qualified HTTP
import qualified Logger
import qualified Network.URI.Extended as URI

data Handle =
    Handle
        { http :: HTTP.Handle
        , hLog :: Logger.Handle
        , lastUpdate :: IORef Integer
        , baseURL :: String
        }

data Request
    = GET URI.URI
    | POST URI.URI L8.ByteString
    deriving (Show)

baseURI :: Handle -> URI.URI
baseURI hAPI = hAPI & http & (HTTP.baseURI :: HTTP.Handle -> URI.URI)

get :: Handle -> URI.URI -> IO L8.ByteString
get hAPI = hAPI & http & HTTP.get'

post :: Handle -> URI.URI -> L8.ByteString -> IO L8.ByteString
post hAPI = hAPI & http & HTTP.post'

sendRequest :: Handle -> Request -> IO L8.ByteString
sendRequest hAPI req =
    case req of
        GET method -> get hAPI method
        POST method body -> post hAPI method body
