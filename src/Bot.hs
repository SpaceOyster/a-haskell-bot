module Bot where

import qualified API (Handle)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)

data Handle m =
    Handle
        { hAPI :: API.Handle m
        , state :: IORef BotState
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        , defaultRepeat :: Int
        }

hGetState :: Handle m -> IO BotState
hGetState = readIORef . state

hSetState :: Handle m -> (BotState -> BotState) -> IO ()
hSetState hAPI f = state hAPI `modifyIORef` f

data BotState =
    BotState
        { userSettings :: Map L8.ByteString Int
        }
