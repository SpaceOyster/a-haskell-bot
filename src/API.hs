{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module API
    ( Handle(..)
    , Request(..)
    , sendRequest
    , getState
    , setState
    , modifyState
    , StatefullAPI(..)
    ) where

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef
import qualified Exceptions as Ex
import qualified HTTP
import qualified Logger
import qualified Network.URI.Extended as URI

data Handle state =
    Handle
        { http :: HTTP.Handle
        , hLog :: Logger.Handle
        , baseURI :: URI.URI
        , apiState :: IORef state
        , apiState2 :: IORef PollCreds
        }

data PollCreds =
    PollCreds
        { pollURI :: URI.URI
        , queryParams :: URI.QueryParams
        , body :: L8.ByteString
        }

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

get :: Handle s -> URI.URI -> IO L8.ByteString
get hAPI = hAPI & http & HTTP.get'

post :: Handle s -> URI.URI -> L8.ByteString -> IO L8.ByteString
post hAPI = hAPI & http & HTTP.post'

sendRequest :: Handle s -> Request -> IO L8.ByteString
sendRequest hAPI@Handle {hLog} req = do
    Logger.debug' hLog $ "Vkontakte: sending request: " <> show req
    res <-
        case req of
            GET method -> get hAPI method
            POST method body -> post hAPI method body
    Logger.debug' hLog $ "Vkontakte: got response: " <> L8.unpack res
    pure res

getState :: Handle s -> IO s
getState = readIORef . apiState

modifyState :: Handle s -> (s -> s) -> IO ()
modifyState hAPI morph = apiState hAPI `modifyIORef'` morph

setState :: Handle s -> s -> IO ()
setState hAPI newState = modifyState hAPI $ const newState

class StatefullAPI h where
getState2 :: Handle s -> IO PollCreds
getState2 = readIORef . apiState2

modifyState2 :: Handle s -> (PollCreds -> PollCreds) -> IO ()
modifyState2 hAPI morph = apiState2 hAPI `modifyIORef'` morph

setState2 :: Handle s -> PollCreds -> IO ()
setState2 hAPI newState = modifyState2 hAPI $ const newState

    type Response h
    type Method h
    runMethod :: h -> Method h -> IO (Response h)
