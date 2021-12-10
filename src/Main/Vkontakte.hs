{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main.Vkontakte where

import qualified Bot
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson.Extended as A (throwDecode)
import Data.AppConfig
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text.Extended as T
import Handle.Class (IsHandle(..))
import qualified Logger as L
import qualified System.Environment as E
import qualified System.Exit as Exit (die)

loop :: (Bot.BotHandle a) => a -> Int -> IO ()
loop hBot period = forever $ Bot.doBotThing hBot >> threadDelay period

run :: AppConfig -> IO ()
run AppConfig {..} = do
    L.withHandle logger $ \hLog -> do
        L.logInfo hLog "Initiating Main Bot loop"
        L.logInfo hLog $
            "API Polling period is " <>
            T.tshow (fromIntegral poll_period / 1000 :: Double) <> "ms"
        withHandle vkontakte hLog $ flip loop poll_period

main :: IO ()
main = do
    args <- E.getArgs
    case args of
        cfgPath:_as -> readConfig cfgPath >>= run
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
