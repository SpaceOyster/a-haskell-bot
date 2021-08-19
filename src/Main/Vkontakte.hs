module Main.Vkontakte where

import qualified API.Vkontakte as VK (VKState)
import qualified Bot
import qualified Bot.Vkontakte as VK
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Logger

loop :: Bot.Handle VK.VKState -> Int -> IO ()
loop hBot period =
    forever $ do
        VK.doBotThing hBot
        threadDelay period

run :: VK.Config -> IO ()
run vkontakte = do
    let logger = Logger.Config Nothing Logger.Debug
    let poll_period = 5 * 1000 * 1000
    Logger.withHandle logger $ \hLog -> do
        VK.withHandle vkontakte hLog $ flip loop poll_period
