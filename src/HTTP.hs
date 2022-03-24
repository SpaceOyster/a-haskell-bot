{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP
  ( Config (..),
    Handle (..),
    HTTP.Request (..),
    new,
  )
where

import App.Error as Err
import Control.Monad.Catch (MonadThrow, catch, throwM)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Has (Has (..))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP (Request (..))
import qualified Network.HTTP.Client as H
  ( HttpException,
    Manager,
    Request (..),
    RequestBody (..),
    httpLbs,
    newManager,
    requestFromURI,
    responseBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.URI.Extended as URI

data Config = Config
  {
  }

newtype Handle = Handle
  { sendRequest :: HTTP.Request -> IO L8.ByteString
  }

instance Has Handle Handle where
  obtain = id

new :: Config -> IO Handle
new _cfg = do
  manager <- H.newManager tlsManagerSettings
  pure $ Handle {sendRequest = \req -> sendWith manager req `catch` rethrow}
  where
    rethrow :: MonadThrow m => H.HttpException -> m a
    rethrow = throwM . httpToAppError

sendWith :: H.Manager -> HTTP.Request -> IO L8.ByteString
sendWith manager req =
  case req of
    HTTP.GET uri -> get manager uri
    HTTP.POST uri body -> post manager uri body

httpToAppError :: H.HttpException -> Err.AppError
httpToAppError = HTTPError . T.tshow

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
        { H.method = "POST",
          H.requestBody = H.RequestBodyLBS body,
          H.requestHeaders =
            [("Content-Type", "application/json; charset=utf-8")]
        }
