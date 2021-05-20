module Bot where

import qualified API
import qualified API.Telegram as TG
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef
import Utils

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
