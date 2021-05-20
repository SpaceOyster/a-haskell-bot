module Main.Telegram where

import Bot.Telegram
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

loop :: Handle IO BotState -> IO ()
loop hBot =
    forever $ do
        doBotThing hBot
        threadDelay 5000000
