{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import App.Config
import qualified App.Monad as App
import qualified Bot
import qualified Bot.Telegram as TG
import qualified Bot.Vkontakte as VK
import Control.Applicative ((<|>))
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson.Extended as A (throwDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Text.Extended as T
import qualified HTTP
import qualified Logger
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

data BotToRun
    = Telegram
    | Vkontakte

main :: IO ()
main =
    flip (<|>) (Exit.die usagePrompt) $ do
        bot:cfgPath:_as <- E.getArgs
        cfg <- readConfig cfgPath
        botToRun <- readBotName bot
        runWithApp cfg botToRun

readBotName :: (MonadFail m) => String -> m BotToRun
readBotName str =
    case toLower <$> str of
        "telegram" -> pure Telegram
        "vkontakte" -> pure Vkontakte
        _ -> fail "unknown bot type"

readConfig :: FilePath -> IO AppConfig
readConfig cfgPath = do
    json <- BL.readFile cfgPath
    A.throwDecode json

usagePrompt :: String
usagePrompt =
    intercalate
        "\n"
        [ "a-haskell-bot - Echo bot for Telegram and Vkontakte, written in Haskell"
        , mempty
        , "Usage: a-haskell-bot BOT FILE"
        , mempty
        , "FILE - is a config file formatted as json, see example config here:"
        , mempty
        , "Available bots:"
        , "  telegram - run bot for Telegram"
        , "  vkontakte - run bot for Vkontakte "
        , "INSTRUCTIONS TO FIND EXAMPLE CONFIG"
        ]

runWithApp :: AppConfig -> BotToRun -> IO ()
runWithApp AppConfig {..} bot =
    Logger.withHandle logger $ \hLog -> do
        Logger.logInfo hLog "Initiating Main Bot loop"
        Logger.logInfo hLog $
            "API Polling period is " <>
            T.tshow (fromIntegral poll_period / 1000 :: Double) <> "ms"
        hHTTP <- HTTP.new HTTP.Config {}
        let env = App.Env {envLogger = hLog, envHTTP = hHTTP}
        app <-
            case bot of
                Telegram -> flip Bot.loop poll_period <$> TG.new telegram hLog
                Vkontakte ->
                    flip Bot.loop poll_period <$> VK.new vkontakte hLog hHTTP
        App.unApp app `runReaderT` env
