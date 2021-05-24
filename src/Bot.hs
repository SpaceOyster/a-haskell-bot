module Bot where

import qualified API (Handle)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)

data Handle m state =
    Handle
        { hAPI :: API.Handle m
        , state :: IORef state
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        , defaultRepeat :: Int
        }

hGetState :: Handle m state -> IO state
hGetState = readIORef . state

hSetState :: Handle m state -> (state -> state) -> IO ()
hSetState hAPI f = state hAPI `modifyIORef` f

data BotState =
    BotState
        { userSettings :: Map L8.ByteString Int
        }
