module Spec where

import qualified Network.URI.ExtendedSpec (spec)
import Test.Hspec ( hspec, Spec )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Network.URI.ExtendedSpec.spec
