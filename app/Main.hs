module Main where

import qualified API.Telegram as Telegram
import qualified Bot

main :: IO ()
main = do
  telegramConfig <- Telegram.parseConfig
  Bot.loop =<< Telegram.new telegramConfig
