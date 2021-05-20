module Main.Telegram where

import Bot.Telegram
import Control.Concurrent (threadDelay)

loop :: Handle IO BotState -> IO ()
loop hBot = do
    doBotThing hBot
    threadDelay 5000000
    loop hBot
