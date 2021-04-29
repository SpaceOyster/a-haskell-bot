module Bot where

import qualified API
import qualified API.Telegram as TG
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import HTTP
import Utils

doBotThing :: API.Handle IO -> IO [L8.ByteString]
doBotThing hAPI = do
    json <- hAPI & API.sendRequest $ hAPI & API.getUpdates
    requests <- TG.reactToUpdates hAPI json
    mapM (API.sendRequest hAPI) requests

loop :: API.Handle IO -> IO ()
loop hAPI = do
    doBotThing hAPI
    threadDelay 5000000
    loop hAPI
