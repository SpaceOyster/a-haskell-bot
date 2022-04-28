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
  { repliesM :: BR.RepliesM,
    defaultEchoMultiplier :: Int,
    logger :: Logger.Config,
    telegram :: TG.Config,
    vkontakte :: VK.Config
  }
  deriving (Show)

mergeStringsVK :: VK.Config -> BR.RepliesM -> VK.Config
mergeStringsVK cfg ss = cfg {VK.repliesM = VK.repliesM cfg <> ss}

instance A.FromJSON AppConfig where
  parseJSON =
    A.withObject "FromJSON Main.AppConfig" $ \o -> do
      defaults <- o A..: "defaults"
      defaultEchoMultiplier <- defaults A..:? "default-echo-multiplier" A..!= 1
      repliesM <- mempty <|> o A..: "replies" >>= parseRepliesM
      logger <- o A..:? "logger" A..!= mempty
      vkontakte' <- o A..: "vkontakte" >>= parseVKConfig
      let vkontakte = vkontakte' `mergeStringsVK` repliesM
      telegram <- o A..: "telegram" >>= parseTGConfig
      pure $ AppConfig {..}

parseTGConfig :: A.Value -> A.Parser TG.Config
parseTGConfig =
  A.withObject "FromJSON Bot.Telegram" $ \o -> do
    defaultEchoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
    key <- o A..:? "api-key" A..!= ""
    timeout_seconds <- o A..:? "timeout-seconds" A..!= 100
    pure $ TG.Config {..}

parseVKConfig :: A.Value -> A.Parser VK.Config
parseVKConfig =
  A.withObject "FromJSON Bot.Telegram" $ \o -> do
    defaultEchoMultiplier <- o A..:? "default-echo-multiplier" A..!= 1
    key <- o A..:? "api-key" A..!= ""
    repliesM <- mempty <|> o A..: "replies" >>= parseRepliesM
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
