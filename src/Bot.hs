module Bot where

import API.Telegram
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as L8

doBotThing :: Handle -> IO [L8.ByteString]
doBotThing handle = getUpdates handle >>= reactToUpdates handle

loop :: Handle -> IO ()
loop handle = do
    doBotThing handle
    threadDelay 5000000
    loop handle
