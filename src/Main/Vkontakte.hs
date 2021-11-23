{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Vkontakte where

import qualified Bot
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson.Extended as A (throwDecode)
import Data.AppConfig
import qualified Data.ByteString.Lazy as BL
import Handle.Class (IsHandle(..))
import qualified Logger

loop :: (Bot.BotHandle a) => a -> Int -> IO ()
loop hBot period =
    forever $ do
        Bot.doBotThing hBot
        threadDelay period

run :: FilePath -> IO ()
run configPath = do
    json <- BL.readFile configPath
    AppConfig {..} <- A.throwDecode json
    Logger.withHandle logger $ \hLog -> do
        Logger.info' hLog "Initiating Main Bot loop"
        Logger.info' hLog $
            "API Polling period is " <>
            show (fromIntegral poll_period / 1000) <> "ms"
        withHandle vkontakte hLog $ flip loop poll_period
