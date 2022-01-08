{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main.Telegram where

import App.Config
import qualified App.Monad as App
import qualified Bot
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import qualified Data.Aeson.Extended as A (throwDecode)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text.Extended as T
import qualified HTTP
import Handle.Class (IsHandle(..))
import qualified Logger as L
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

loop ::
       ( MonadIO m
       , MonadThrow m
       , Bot.BotHandle a
       , MonadReader env m
       , App.Has L.Handle env
       , App.Has HTTP.Handle env
       )
    => a
    -> Int
    -> m ()
loop hBot period = forever $ Bot.doBotThing hBot >> liftIO (threadDelay period)

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
runWithApp AppConfig {..} = do
    L.withHandle logger $ \hLog -> do
        L.logInfo hLog "Initiating Main Bot loop"
        L.logInfo hLog $
            "API Polling period is " <>
            T.tshow (fromIntegral poll_period / 1000 :: Double) <> "ms"
        hHTTP <- HTTP.new HTTP.Config {}
        let env = App.Env {envLogger = hLog, envHTTP = hHTTP}
        hBot <- new telegram hLog
        App.unApp (loop hBot poll_period) `runReaderT` env
