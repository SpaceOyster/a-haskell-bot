{-# LANGUAGE OverloadedStrings #-}
module Data.Text.ExtendedSpec (spec) where

import Test.Hspec (Spec,describe, it, shouldBe, context, pending, shouldReturn)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Extended

spec :: Spec
spec = describe "Data.Text.Extended" $ do
  tshowSpec
  lazyDecodeUtf8Spec

tshowSpec :: Spec
tshowSpec = describe "tshow" $ do
  it "converts type of Show class to Text" $ do
    tshow (Just "some") `shouldBe` ("Just \"some\"" :: Text)
    tshow (Just [1,2,3,4]) `shouldBe` ("Just [1,2,3,4]" :: Text)
    tshow [1,2,3,4] `shouldBe` ("[1,2,3,4]" :: Text)
    tshow 1234 `shouldBe` ("1234" :: Text)

lazyDecodeUtf8Spec :: Spec
lazyDecodeUtf8Spec = describe "lazyDecodeUtf8" $ do
  it "converts lazy Char8.ByteString to Text" $ do
    lazyDecodeUtf8 ("Some Text" :: L8.ByteString) `shouldBe` ("Some Text" :: Text)
    lazyDecodeUtf8 ("" :: L8.ByteString) `shouldBe` ("" :: Text)
