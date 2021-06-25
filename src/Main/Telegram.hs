{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Telegram where

import qualified Bot
import qualified Bot.Telegram as TG
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Logger
import qualified Utils as U (throwDecode)

loop :: Bot.Handle IO -> Int -> IO ()
loop hBot period =
    forever $ do
        TG.doBotThing hBot
        threadDelay period

run :: FilePath -> IO ()
run configPath = do
    json <- BL.readFile configPath
    AppConfig {..} <- U.throwDecode json
    TG.withHandle telegram $ flip loop poll_period

data AppConfig =
    AppConfig
        { poll_period :: Int
        , strings :: Bot.Strings
        , logger :: Logger.Config
        , telegram :: TG.Config
        }
    deriving (Show)

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            poll_period_ms <- defaults A..: "poll_period_ms"
            let poll_period = (1000 *) $ max 500 poll_period_ms
            strings <- o A..:? "strings" A..!= mempty
            logger <- o A..:? "logger" A..!= mempty
            telegram' <- o A..: "telegram"
            let telegram = telegram' `TG.mergeStrings` strings
            return $ AppConfig {..}

instance A.FromJSON TG.Config where
    parseJSON =
        A.withObject "FromJSON Bot.Telegram" $ \o -> do
            echoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
            key <- o A..:? "api_key" A..!= ""
            strings <- o A..:? "strings" A..!= mempty
            return $ TG.Config {..}

instance A.FromJSON Bot.Strings where
    parseJSON =
        A.withObject "" $ \o -> do
            helpM <- o A..:? "help"
            greetingM <- o A..:? "greeting"
            repeatM <- o A..:? "repeat"
            return $ Bot.Strings {..}
