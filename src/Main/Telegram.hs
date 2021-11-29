{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Telegram where

import qualified Bot
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson.Extended as A (throwDecode)
import Data.AppConfig
import qualified Data.ByteString.Lazy as BL
import Handle.Class (IsHandle(..))
import qualified Logger as L

loop :: (Bot.BotHandle a) => a -> Int -> IO ()
loop hBot period =
    forever $ do
        Bot.doBotThing hBot
        threadDelay period

run :: FilePath -> IO ()
run configPath = do
    json <- BL.readFile configPath
    AppConfig {..} <- A.throwDecode json
    L.withHandle logger $ \hLog -> do
        L.logInfo' hLog "Initiating Main Bot loop"
        L.logInfo' hLog $
            "API Polling period is " <>
            show (fromIntegral poll_period / 1000) <> "ms"
        withHandle telegram hLog $ flip loop poll_period
