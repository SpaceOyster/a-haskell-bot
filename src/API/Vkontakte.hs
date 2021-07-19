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
    baseURI <- makeBaseURI http cfg
    lastUpdate <- newIORef 0
    pure $ Handle {http, hLog = undefined, lastUpdate, baseURI}

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

makeBaseURI :: HTTP.Handle -> Config -> IO URI.URI
makeBaseURI http cfg@Config {..} = do
    res <- getLongPollServer http cfg
    let PollServer {..} = response res
    maybe ex pure . URI.parseURI $
        server <> "/?act=a_check&key=" <> key <> "&ts=" <> show ts <> "&wait=25"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Vkontakte Long Poll URL"

getLongPollServer :: HTTP.Handle -> Config -> IO APIResponse
getLongPollServer http Config {..} = do
    json <-
        http & HTTP.get $
        "https://api.vk.com/method/groups.getLongPollServer?v=" <>
        v <> "&access_token=" <> key <> "&group_id=" <> show group_id
    throwDecode json
