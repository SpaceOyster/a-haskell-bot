module Main.Telegram where

import qualified Bot
import qualified Bot.Telegram as TG
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

loop :: Bot.Handle IO -> IO ()
loop hBot =
    forever $ do
        TG.doBotThing hBot
        threadDelay 5000000

data AppConfig =
    AppConfig
        { poll_period :: Integer
        , telegram :: TG.Config
        }
