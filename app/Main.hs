module Main where

import qualified Bot.Telegram as TG (withHandle)
import qualified Main.Telegram as TG

main :: IO ()
main = do
  TG.withHandle TG.loop
