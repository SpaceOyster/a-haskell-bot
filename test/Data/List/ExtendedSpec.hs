module Data.List.ExtendedSpec
  ( spec
  ) where

import Data.List.Extended (replaceSubseq)
import Test.Hspec

spec :: Spec
spec = replaceSubseqSpec

replaceSubseqSpec :: Spec
replaceSubseqSpec = do
  describe "Data.List.Extended.replaceSubseq" $ do
    it "replaces subsequence of list with new subsequence" $ do
      replaceSubseq [1 .. 10] [3, 4, 5] [20, 20] `shouldBe`
        [1, 2, 20, 20, 6, 7, 8, 9, 10]
      replaceSubseq "some %n string" "%n" "nice" `shouldBe` "some nice string"
    it "doesn't change list is required to replace an empty subsequence" $ do
      let testLine = "Any String Here For test"
      replaceSubseq testLine "" "whatever" `shouldBe` testLine
    it "removes subsequence if new one is empty list" $ do
      replaceSubseq [1 .. 10] [2 .. 7] [] `shouldBe` [1, 8, 9, 10]
