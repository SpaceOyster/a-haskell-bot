{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Vkontakte
    (
    ) where

import API
import Data.Aeson
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.Generics
import qualified HTTP
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
    getLongPollServer http cfg
    lastUpdate <- newIORef 0
    pure $ Handle {http, hLog = undefined, lastUpdate, baseURI = undefined}

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

getLongPollServer :: HTTP.Handle -> Config -> IO APIResponse
getLongPollServer http Config {..} = do
    json <-
        http & HTTP.get $
        "https://api.vk.com/method/groups.getLongPollServer?v=" <>
        v <> "&access_token=" <> key <> "&group_id=" <> show group_id
    throwDecode json
