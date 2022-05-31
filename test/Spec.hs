module Spec where

import qualified API.TelegramSpec (spec)
import qualified Data.Text.ExtendedSpec (spec)
import qualified Effects.BotRepliesSpec (spec)
import qualified Effects.UsersDBSpec (spec)
import qualified Network.URI.ExtendedSpec (spec)
import Test.Hspec (Spec, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Network.URI.ExtendedSpec.spec
  Effects.BotRepliesSpec.spec
  Effects.UsersDBSpec.spec
  Data.Text.ExtendedSpec.spec
  API.TelegramSpec.spec
