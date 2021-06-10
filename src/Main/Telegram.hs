{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
        { poll_period :: Integer
        , telegram :: TG.Config
        }

instance A.FromJSON AppConfig where
    parseJSON =
        A.withObject "FromJSON Main.AppConfig" $ \o -> do
            defaults <- o A..: "defaults"
            repeats <- defaults A..: "repeats"
            poll_period <- defaults A..: "poll-period"
            telegramO <- o A..: "telegram"
            tg_api_key <- telegramO A..: "api-key"
            strings <- o A..: "strings"
            help <- strings A..: "help"
            greeting <- strings A..: "greeting"
            repeat <- strings A..: "repeat"
            let telegram =
                    TG.Config
                        { TG.key = tg_api_key
                        , TG.helpMessage = help
                        , TG.greeting = greeting
                        , TG.repeatPrompt = repeat
                        , TG.defaultRepeat = repeats
                        }
            return $ AppConfig {..}
