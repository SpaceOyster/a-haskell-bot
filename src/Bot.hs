module Bot where

import qualified API (Handle)
import Data.IORef

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
