{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config
  ( AppConfig (..),
  )
where

import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser, parseMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import qualified Effects.BotReplies as BR (Replies (..))
import qualified Handlers.Logger as Logger

data AppConfig = AppConfig
  { replies :: BR.Replies,
    defaultEchoMultiplier :: Int,
    logger :: Logger.Config,
    telegramM :: Maybe TG.Config,
    vkontakteM :: Maybe VK.Config
  }
  deriving (Show)

instance A.FromJSON AppConfig where
  parseJSON =
    A.withObject "FromJSON Main.AppConfig" $ \o -> do
      defaults <- o A..: "defaults"
      defaultEchoMultiplier <- defaults A..:? "default-echo-multiplier" A..!= 1
      replies <- o A..: "replies" >>= parseReplies
      logger <- o A..:? "logger" A..!= mempty
      let telegramM = A.parseMaybe parseTGConfig (A.Object o)
      let vkontakteM = A.parseMaybe parseVKConfig (A.Object o)
      pure $ AppConfig {..}

parseTGConfig :: A.Value -> A.Parser TG.Config
parseTGConfig =
  A.withObject "FromJSON Bot.Telegram" $ \obj -> do
    o <- obj A..: "telegram"
    key <- o A..:? "api-key" A..!= ""
    timeout_seconds <- o A..:? "timeout-seconds" A..!= 100
    pure $ TG.Config {..}

parseVKConfig :: A.Value -> A.Parser VK.Config
parseVKConfig =
  A.withObject "FromJSON Bot.Vkontakte" $ \obj -> do
    o <- obj A..: "vkontakte"
    key <- o A..:? "api-key" A..!= ""
    group_id <- o A..:? "group-id" A..!= 0
    v <- o A..:? "api-version" A..!= "5.86"
    wait_seconds <- o A..:? "wait-seconds" A..!= 25
    pure $ VK.Config {..}

parseReplies :: A.Value -> A.Parser BR.Replies
parseReplies =
  A.withObject "AppConfig.replies" $ \o -> do
    helpM <- o A..:? "help"
    greetingM <- o A..:? "greeting"
    repeatM <- o A..:? "repeat"
    unknownM <- o A..:? "unknown"
    settingsSavedM <- o A..:? "settings-saved"
    pure $ fromRepliesM $ RepliesM {..}

data RepliesM = RepliesM
  { helpM :: Maybe T.Text,
    greetingM :: Maybe T.Text,
    repeatM :: Maybe T.Text,
    unknownM :: Maybe T.Text,
    settingsSavedM :: Maybe T.Text
  }
  deriving (Show)

fromRepliesM :: RepliesM -> BR.Replies
fromRepliesM RepliesM {..} =
  BR.Replies
    { help = fromMaybe "" helpM,
      greeting = fromMaybe "" greetingM,
      repeat = fromMaybe "" repeatM,
      unknown = fromMaybe "" unknownM,
      settingsSaved = fromMaybe "" settingsSavedM
    }
