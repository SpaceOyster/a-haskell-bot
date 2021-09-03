{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Telegram where

import qualified API.Telegram as TG (APIState)
import qualified Bot
import qualified Bot.Telegram as TG (doBotThing, withHandle)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.AppConfig
import qualified Data.ByteString.Lazy as BL
import qualified Logger
import qualified Utils as U (throwDecode)

loop :: Bot.Handle TG.APIState -> Int -> IO ()
loop hBot period =
    forever $ do
        TG.doBotThing hBot
        threadDelay period

run :: FilePath -> IO ()
run configPath = do
    json <- BL.readFile configPath
    AppConfig {..} <- U.throwDecode json
    Logger.withHandle logger $ \hLog -> do
        Logger.info' hLog "Initiating Main Bot loop"
        Logger.info' hLog $
            "API Polling period is " <>
            show (fromIntegral poll_period / 1000) <> "ms"
        TG.withHandle telegram hLog $ flip loop poll_period
