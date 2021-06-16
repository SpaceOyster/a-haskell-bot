module Bot where

import qualified API (Handle)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)

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
    deriving (Show)

hGetState :: Handle m -> IO BotState
hGetState = readIORef . state

hSetState :: Handle m -> (BotState -> BotState) -> IO ()
hSetState hAPI f = state hAPI `modifyIORef` f

type Hash = L8.ByteString

newtype BotState =
    BotState
        { userSettings :: Map Hash Int
        }
