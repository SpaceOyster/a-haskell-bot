{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Bot where

import qualified API (Handle)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)
import GHC.Generics (Generic)

data Handle m =
    Handle
        { hAPI :: API.Handle m
        , state :: IORef BotState
        , strings :: Strings
        , echoMultiplier :: Int
        }

data Strings =
    Strings
        { help :: String
        , greeting :: String
        , repeat :: String
        }
    deriving (Show, Generic, FromJSON)

hGetState :: Handle m -> IO BotState
hGetState = readIORef . state

hSetState :: Handle m -> (BotState -> BotState) -> IO ()
hSetState hAPI f = state hAPI `modifyIORef` f

type Hash = L8.ByteString

newtype BotState =
    BotState
        { userSettings :: Map Hash Int
        }
