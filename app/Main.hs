module Main where

import qualified API.Telegram as Telegram
import qualified Bot
import HTTP

main :: IO ()
main = do
  telegramConfig <- HTTP.parseConfig
  Bot.loop =<< HTTP.new telegramConfig
