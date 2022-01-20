{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Config
    ( AppConfig(..)
    ) where

import qualified Bot
import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser)
import qualified Logger

data AppConfig =
    AppConfig
        { poll_period :: Int
        , stringsM :: Bot.StringsM
        , defaultEchoMultiplier :: Int
        , logger :: Logger.Config
        , telegram :: TG.Config
        , vkontakte :: VK.Config
        }
    deriving (Show)

mergeStringsTG :: TG.Config -> Bot.StringsM -> TG.Config
mergeStringsTG cfg ss = cfg {TG.stringsM = TG.stringsM cfg <> ss}

mergeStringsVK :: VK.Config -> Bot.StringsM -> VK.Config
mergeStringsVK cfg ss = cfg {VK.stringsM = VK.stringsM cfg <> ss}

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            defaultEchoMultiplier <-
                defaults A..:? "default-echo-multiplier" A..!= 1
            poll_period_ms <- defaults A..: "poll-period-ms"
            let poll_period = (1000 *) $ max 500 poll_period_ms
            stringsM <- mempty <|> o A..: "strings" >>= parseStringsM
            logger <- o A..:? "logger" A..!= mempty
            telegram' <- o A..: "telegram" >>= parseTGConfig
            let telegram = telegram' `mergeStringsTG` stringsM
            vkontakte' <- o A..: "vkontakte" >>= parseVKConfig
            let vkontakte = vkontakte' `mergeStringsVK` stringsM
            pure $ AppConfig {..}

parseTGConfig :: A.Value -> A.Parser TG.Config
parseTGConfig =
    A.withObject "FromJSON Bot.Telegram" $ \o -> do
        defaultEchoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
        key <- o A..:? "api-key" A..!= ""
        stringsM <- mempty <|> o A..: "strings" >>= parseStringsM
        pure $ TG.Config {..}

parseVKConfig :: A.Value -> A.Parser VK.Config
parseVKConfig =
    A.withObject "FromJSON Bot.Telegram" $ \o -> do
        defaultEchoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
        key <- o A..:? "api-key" A..!= ""
        stringsM <- mempty <|> o A..: "strings" >>= parseStringsM
        group_id <- o A..:? "group-id" A..!= 0
        v <- o A..:? "api-version" A..!= "5.86"
        pure $ VK.Config {..}

parseStringsM :: A.Value -> A.Parser Bot.StringsM
parseStringsM =
    A.withObject "AppConfig.strings" $ \o -> do
        helpM <- o A..:? "help"
        greetingM <- o A..:? "greeting"
        repeatM <- o A..:? "repeat"
        unknownM <- o A..:? "unknown"
        settingsSavedM <- o A..:? "settings-saved"
        pure $ Bot.StringsM {..}
