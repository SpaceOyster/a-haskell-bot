{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Vkontakte
    (
    ) where

import API
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Exceptions as Ex
import GHC.Generics
import qualified HTTP
import qualified Network.URI.Extended as URI
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , group_id :: Integer
        , v :: String
        }

new :: Config -> IO Handle
new cfg@Config {..} = do
    http <- HTTP.new $ HTTP.Config {}
    pollServer <- getLongPollServer http cfg
    baseURI <- makeBaseURI pollServer
    lastUpdate <- newIORef $ ts pollServer
    pure $ Handle {http, hLog = undefined, lastUpdate, baseURI}

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config io = do
    hAPI <- new config
    io hAPI

data PollServer =
    PollServer
        { key :: String
        , server :: String
        , ts :: Integer
        }
    deriving (Show, Generic, FromJSON)

data APIResponse =
    Response
        { response :: PollServer
        }
    deriving (Show, Generic, FromJSON)

makeBaseURI :: MonadThrow m => PollServer -> m URI.URI
makeBaseURI PollServer {..} = do
    maybe ex pure . URI.parseURI $
        server <> "/?act=a_check&key=" <> key <> "&wait=25"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte Long Poll URL"

getLongPollServer :: HTTP.Handle -> Config -> IO PollServer
getLongPollServer http Config {..} = do
    json <-
        http & HTTP.get $
        "https://api.vk.com/method/groups.getLongPollServer?v=" <>
        v <> "&access_token=" <> key <> "&group_id=" <> show group_id
    res <- throwDecode json
    pure $ response res
