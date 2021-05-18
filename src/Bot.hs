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
        { api :: API.Handle m state
        , state :: IORef state
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        , defaultRepeat :: Int
        }

doBotThing :: API.Handle IO TG.HState -> IO [L8.ByteString]
doBotThing hAPI = do
    req <- hAPI & TG.getUpdates
    json <- hAPI & API.sendRequest $ req
    requests <- TG.reactToUpdates hAPI json
    mapM (API.sendRequest hAPI) requests

loop :: API.Handle IO TG.HState -> IO ()
loop hAPI = do
    doBotThing hAPI
    threadDelay 5000000
    loop hAPI
