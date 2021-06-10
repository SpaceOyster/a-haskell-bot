{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main.Telegram where

import qualified Bot
import qualified Bot.Telegram as TG
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL

loop :: Bot.Handle IO -> IO ()
loop hBot =
    forever $ do
        TG.doBotThing hBot
        threadDelay 5000000

data AppConfig =
    AppConfig
        { poll_period :: Int
        , telegram :: TG.Config
        }

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            poll_period_ms <- defaults A..: "poll-period"
            let poll_period = (1000 *) $ max 500 poll_period_ms
            telegram <- A.parseJSON (A.Object o)
            return $ AppConfig {..}

instance A.FromJSON TG.Config where
    parseJSON =
        A.withObject "FromJSON Bot.Telegram" $ \o -> do
            defaults <- o A..: "defaults"
            defaultRepeat <- defaults A..: "repeats"
            telegramO <- o A..: "telegram"
            key <- telegramO A..: "api-key"
            strings <- o A..: "strings"
            helpMessage <- strings A..: "help"
            greeting <- strings A..: "greeting"
            repeatPrompt <- strings A..: "repeat"
            return $ TG.Config {..}
