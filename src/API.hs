{-# LANGUAGE DuplicateRecordFields #-}

module API
    ( Handle(..)
    , Request(..)
    , sendRequest
    , getLastUpdateID
    , setLastUpdateID
    , APIState(..)
    , getState
    , setState
    ) where

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef
import qualified Exceptions as Ex
import qualified HTTP
import qualified Logger
import qualified Network.URI.Extended as URI

data Handle =
    Handle
        { http :: HTTP.Handle
        , hLog :: Logger.Handle
        , lastUpdateID :: IORef Integer
        , baseURI :: URI.URI
        , apiState :: IORef APIState
        }

data APIState
    = VK
          { vkLastUpdateID :: String
          }
    | TG
          { tgLastUpdateID :: Integer
          }

data Request
    = GET URI.URI
    | POST URI.URI L8.ByteString
    deriving (Show)

get :: Handle -> URI.URI -> IO L8.ByteString
get hAPI = hAPI & http & HTTP.get'

post :: Handle -> URI.URI -> L8.ByteString -> IO L8.ByteString
post hAPI = hAPI & http & HTTP.post'

sendRequest :: Handle -> Request -> IO L8.ByteString
sendRequest hAPI req =
    case req of
        GET method -> get hAPI method
        POST method body -> post hAPI method body

getLastUpdateID :: Handle -> IO Integer
getLastUpdateID = readIORef . lastUpdateID

setLastUpdateID :: Handle -> Integer -> IO ()
setLastUpdateID hAPI id = lastUpdateID hAPI `modifyIORef'` const id

getState :: Handle -> IO APIState
getState = readIORef . apiState

setState :: Handle -> APIState -> IO ()
setState hAPI newState = do
    oldState <- getState hAPI
    case (oldState, newState) of
        (VK _, VK _) -> doSetState
        (TG _, TG _) -> doSetState
        _ -> throwM $ Ex.APIStateSetting "State type is not compatible"
  where
    doSetState = apiState hAPI `modifyIORef'` const newState
