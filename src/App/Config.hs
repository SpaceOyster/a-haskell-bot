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
    , repliesM :: Bot.RepliesM
    , defaultEchoMultiplier :: Int
    , logger :: Logger.Config
    , telegram :: TG.Config
    , vkontakte :: VK.Config
    }
  deriving (Show)

mergeStringsTG :: TG.Config -> Bot.RepliesM -> TG.Config
mergeStringsTG cfg ss = cfg {TG.repliesM = TG.repliesM cfg <> ss}

mergeStringsVK :: VK.Config -> Bot.RepliesM -> VK.Config
mergeStringsVK cfg ss = cfg {VK.repliesM = VK.repliesM cfg <> ss}

instance A.FromJSON AppConfig where
  parseJSON =
    A.withObject "FromJSON Main.AppConfig" $ \o -> do
      defaults <- o A..: "defaults"
      defaultEchoMultiplier <- defaults A..:? "default-echo-multiplier" A..!= 1
      poll_period_ms <- defaults A..: "poll-period-ms"
      let poll_period = (1000 *) $ max 500 poll_period_ms
      repliesM <- mempty <|> o A..: "strings" >>= parseStringsM
      logger <- o A..:? "logger" A..!= mempty
      telegram' <- o A..: "telegram" >>= parseTGConfig
      let telegram = telegram' `mergeStringsTG` repliesM
      vkontakte' <- o A..: "vkontakte" >>= parseVKConfig
      let vkontakte = vkontakte' `mergeStringsVK` repliesM
      pure $ AppConfig {..}

parseTGConfig :: A.Value -> A.Parser TG.Config
parseTGConfig =
  A.withObject "FromJSON Bot.Telegram" $ \o -> do
    defaultEchoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
    key <- o A..:? "api-key" A..!= ""
    repliesM <- mempty <|> o A..: "strings" >>= parseStringsM
    pure $ TG.Config {..}

parseVKConfig :: A.Value -> A.Parser VK.Config
parseVKConfig =
  A.withObject "FromJSON Bot.Telegram" $ \o -> do
    defaultEchoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
    key <- o A..:? "api-key" A..!= ""
    repliesM <- mempty <|> o A..: "strings" >>= parseStringsM
    group_id <- o A..:? "group-id" A..!= 0
    v <- o A..:? "api-version" A..!= "5.86"
    pure $ VK.Config {..}

parseStringsM :: A.Value -> A.Parser Bot.RepliesM
parseStringsM =
  A.withObject "AppConfig.strings" $ \o -> do
    helpM <- o A..:? "help"
    greetingM <- o A..:? "greeting"
    repeatM <- o A..:? "repeat"
    unknownM <- o A..:? "unknown"
    settingsSavedM <- o A..:? "settings-saved"
    pure $ Bot.RepliesM {..}
