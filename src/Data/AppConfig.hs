{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.AppConfig
    ( AppConfig(..)
    ) where

import qualified Bot
import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import qualified Data.Aeson as A
import qualified Logger

data AppConfig =
    AppConfig
        { poll_period :: Int
        , strings :: Bot.Strings
        , logger :: Logger.Config
        , telegram :: TG.Config
        , vkontakte :: VK.Config
        }
    deriving (Show)

mergeStringsTG :: TG.Config -> Bot.Strings -> TG.Config
mergeStringsTG cfg ss = cfg {TG.strings = TG.strings cfg <> ss}

mergeStringsVK :: VK.Config -> Bot.Strings -> VK.Config
mergeStringsVK cfg ss = cfg {VK.strings = VK.strings cfg <> ss}

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            poll_period_ms <- defaults A..: "poll-period-ms"
            let poll_period = (1000 *) $ max 500 poll_period_ms
            strings <- o A..:? "strings" A..!= mempty
            logger <- o A..:? "logger" A..!= mempty
            telegram' <- o A..: "telegram"
            let telegram = telegram' `mergeStringsTG` strings
            vkontakte' <- o A..: "vkontakte"
            let vkontakte = vkontakte' `mergeStringsVK` strings
            pure $ AppConfig {..}

instance A.FromJSON TG.Config where
    parseJSON =
        A.withObject "FromJSON Bot.Telegram" $ \o -> do
            echoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
            key <- o A..:? "api-key" A..!= ""
            strings <- o A..:? "strings" A..!= mempty
            pure $ TG.Config {..}

instance A.FromJSON VK.Config where
    parseJSON =
        A.withObject "FromJSON Bot.Telegram" $ \o -> do
            echoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
            key <- o A..:? "api-key" A..!= ""
            strings <- o A..:? "strings" A..!= mempty
            group_id <- o A..:? "group-id" A..!= 0
            v <- o A..:? "api-version" A..!= "5.86"
            pure $ VK.Config {..}

instance A.FromJSON Bot.Strings where
    parseJSON =
        A.withObject "" $ \o -> do
            helpM <- o A..:? "help"
            greetingM <- o A..:? "greeting"
            repeatM <- o A..:? "repeat"
            unknownM <- o A..:? "unknown"
            settingsSavedM <- o A..:? "settings-saved"
            pure $ Bot.Strings {..}
