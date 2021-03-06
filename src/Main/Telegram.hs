{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Telegram where

import qualified API.Telegram as TG (APIState)
import qualified Bot
import qualified Bot.Telegram as TG
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as A
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
            "API Pollig period is " <>
            show (fromIntegral poll_period / 1000) <> "ms"
        TG.withHandle telegram hLog $ flip loop poll_period

data AppConfig =
    AppConfig
        { poll_period :: Int
        , strings :: Bot.Strings
        , logger :: Logger.Config
        , telegram :: TG.Config
        }
    deriving (Show)

mergeStrings :: TG.Config -> Bot.Strings -> TG.Config
mergeStrings cfg ss = cfg {TG.strings = TG.strings cfg <> ss}

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            poll_period_ms <- defaults A..: "poll_period_ms"
            let poll_period = (1000 *) $ max 500 poll_period_ms
            strings <- o A..:? "strings" A..!= mempty
            logger <- o A..:? "logger" A..!= mempty
            telegram' <- o A..: "telegram"
            let telegram = telegram' `mergeStrings` strings
            pure $ AppConfig {..}

instance A.FromJSON TG.Config where
    parseJSON =
        A.withObject "FromJSON Bot.Telegram" $ \o -> do
            echoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
            key <- o A..:? "api_key" A..!= ""
            strings <- o A..:? "strings" A..!= mempty
            pure $ TG.Config {..}

instance A.FromJSON Bot.Strings where
    parseJSON =
        A.withObject "" $ \o -> do
            helpM <- o A..:? "help"
            greetingM <- o A..:? "greeting"
            repeatM <- o A..:? "repeat"
            unknownM <- o A..:? "unknown"
            pure $ Bot.Strings {..}
