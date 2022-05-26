module API.TelegramSpec
  ( spec
  ) where

import qualified API.Telegram.TypesSpec (spec)
import qualified API.Telegram.MonadSpec (spec)
import Test.Hspec ( Spec )

spec :: Spec
spec = do
  API.Telegram.TypesSpec.spec
  API.Telegram.MonadSpec.spec
