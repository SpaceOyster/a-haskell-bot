{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Vkontakte where

import qualified API.Vkontakte as VK (VKState)
import qualified Bot
import qualified Bot.Vkontakte as VK
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Logger
import qualified Utils as U (throwDecode)

loop :: Bot.Handle VK.VKState -> Int -> IO ()
loop hBot period =
    forever $ do
        VK.doBotThing hBot
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
        VK.withHandle vkontakte hLog $ flip loop poll_period

data AppConfig =
    AppConfig
        { poll_period :: Int
        , strings :: Bot.Strings
        , logger :: Logger.Config
        , vkontakte :: VK.Config
        }
    deriving (Show)

mergeStrings :: VK.Config -> Bot.Strings -> VK.Config
mergeStrings cfg ss = cfg {VK.strings = VK.strings cfg <> ss}

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            poll_period_ms <- defaults A..: "poll-period-ms"
            let poll_period = (1000 *) $ max 500 poll_period_ms
            strings <- o A..:? "strings" A..!= mempty
            logger <- o A..:? "logger" A..!= mempty
            vkontakte' <- o A..: "vkontakte"
            let vkontakte = vkontakte' `mergeStrings` strings
            pure $ AppConfig {..}

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
