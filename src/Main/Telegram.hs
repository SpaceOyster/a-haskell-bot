{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main.Telegram where

import App.Config
import qualified App.Monad as App
import qualified Bot
import qualified Bot.Telegram as TG
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson.Extended as A (throwDecode)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text.Extended as T
import qualified HTTP
import qualified Logger
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

main :: IO ()
main = do
    args <- E.getArgs
    case args of
        cfgPath:_as -> readConfig cfgPath >>= runWithApp
        _ -> Exit.die usagePrompt

readConfig :: FilePath -> IO AppConfig
readConfig cfgPath = do
    json <- BL.readFile cfgPath
    A.throwDecode json

usagePrompt :: String
usagePrompt =
    intercalate
        "\n"
        [ "telegram-bot - Echo bot for Telegram, written in Haskell"
        , mempty
        , "Usage: telegram-bot FILE"
        , mempty
        , "FILE - is a config file formatted as json, see example config here:"
        , "INSTRUCTIONS TO FIND EXAMPLE CONFIG"
        ]

runWithApp :: AppConfig -> IO ()
runWithApp AppConfig {..} =
    Logger.withHandle logger $ \hLog -> do
        Logger.logInfo hLog "Initiating Main Bot loop"
        Logger.logInfo hLog $
            "API Polling period is " <>
            T.tshow (fromIntegral poll_period / 1000 :: Double) <> "ms"
        hHTTP <- HTTP.new HTTP.Config {}
        let env = App.Env {envLogger = hLog, envHTTP = hHTTP}
        hBot <- TG.new telegram hLog
        App.unApp (Bot.loop hBot poll_period) `runReaderT` env
