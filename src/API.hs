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
    = GET String
    | POST String L8.ByteString
    deriving (Show)

get :: Handle -> String -> IO L8.ByteString
get hAPI m = hAPI & http & HTTP.get $ baseURL hAPI <> m
baseURI :: Handle -> URI.URI
baseURI hAPI = hAPI & http & (HTTP.baseURI :: HTTP.Handle -> URI.URI)

post :: Handle -> String -> L8.ByteString -> IO L8.ByteString
post hAPI m = hAPI & http & HTTP.post $ baseURL hAPI <> m

sendRequest :: Handle -> Request -> IO L8.ByteString
sendRequest hAPI req =
    case req of
        GET method -> get hAPI method
        POST method body -> post hAPI method body
