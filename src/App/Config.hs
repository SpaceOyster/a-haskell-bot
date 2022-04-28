{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config
  ( AppConfig (..),
  )
where

import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser)
import qualified Effects.BotReplies as BR
import qualified Handlers.Logger as Logger

data AppConfig = AppConfig
  { replies :: BR.Replies,
    defaultEchoMultiplier :: Int,
    logger :: Logger.Config,
    telegram :: TG.Config,
    vkontakte :: VK.Config
  }
  deriving (Show)

instance A.FromJSON AppConfig where
  parseJSON =
    A.withObject "FromJSON Main.AppConfig" $ \o -> do
      defaults <- o A..: "defaults"
      defaultEchoMultiplier <- defaults A..:? "default-echo-multiplier" A..!= 1
      replies <- BR.fromRepliesM <$> (o A..: "replies" >>= parseRepliesM)
      logger <- o A..:? "logger" A..!= mempty
      telegram <- o A..: "telegram" >>= parseTGConfig
      vkontakte <- o A..: "vkontakte" >>= parseVKConfig
      pure $ AppConfig {..}

parseTGConfig :: A.Value -> A.Parser TG.Config
parseTGConfig =
  A.withObject "FromJSON Bot.Telegram" $ \o -> do
    key <- o A..:? "api-key" A..!= ""
    timeout_seconds <- o A..:? "timeout-seconds" A..!= 100
    pure $ TG.Config {..}

parseVKConfig :: A.Value -> A.Parser VK.Config
parseVKConfig =
  A.withObject "FromJSON Bot.Telegram" $ \o -> do
    key <- o A..:? "api-key" A..!= ""
    group_id <- o A..:? "group-id" A..!= 0
    v <- o A..:? "api-version" A..!= "5.86"
    wait_seconds <- o A..:? "wait-seconds" A..!= 25
    pure $ VK.Config {..}

parseRepliesM :: A.Value -> A.Parser BR.RepliesM
parseRepliesM =
  A.withObject "AppConfig.replies" $ \o -> do
    helpM <- o A..:? "help"
    greetingM <- o A..:? "greeting"
    repeatM <- o A..:? "repeat"
    unknownM <- o A..:? "unknown"
    settingsSavedM <- o A..:? "settings-saved"
    pure $ BR.RepliesM {..}
