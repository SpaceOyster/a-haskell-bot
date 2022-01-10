{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main.Vkontakte where

import App.Config
import qualified App.Monad as App
import qualified Bot
import qualified Bot.Vkontakte as VK
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
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
        [ "vkontakte-bot - Echo bot for Vkontakte, written in Haskell"
        , mempty
        , "Usage: vkontakte-bot FILE"
        , mempty
        , "FILE - is a config file formatted as json, see example config here:"
        , "INSTRUCTIONS TO FIND EXAMPLE CONFIG"
        ]

runWithApp :: AppConfig -> IO ()
runWithApp AppConfig {..} = do
    Logger.withHandle logger $ \hLog -> do
        Logger.logInfo hLog "Initiating Main Bot loop"
        Logger.logInfo hLog $
            "API Polling period is " <>
            T.tshow (fromIntegral poll_period / 1000 :: Double) <> "ms"
        hHTTP <- HTTP.new HTTP.Config {}
        let env = App.Env {envLogger = hLog, envHTTP = hHTTP}
        hBot <- VK.new vkontakte hLog hHTTP
        App.unApp (Bot.loop hBot poll_period) `runReaderT` env
