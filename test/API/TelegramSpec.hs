module API.TelegramSpec
  ( spec
  ) where

import API.Telegram
import qualified API.Telegram.TypesSpec (spec)
import Test.Hspec

spec :: Spec
spec = do
  API.Telegram.TypesSpec.spec
