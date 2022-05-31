{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ExtendedSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Extended (Text, lazyDecodeUtf8, tshow)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  tshowSpec
  lazyDecodeUtf8Spec

tshowSpec :: Spec
tshowSpec = describe "tshow" $ do
  it "converts type of Show class to Text" $ do
    tshow (Just ("some" :: String)) `shouldBe` ("Just \"some\"" :: Text)
    tshow (Just [1 :: Integer, 2, 3, 4]) `shouldBe` ("Just [1,2,3,4]" :: Text)
    tshow [1 :: Integer, 2, 3, 4] `shouldBe` ("[1,2,3,4]" :: Text)
    tshow (1234 :: Integer) `shouldBe` ("1234" :: Text)

lazyDecodeUtf8Spec :: Spec
lazyDecodeUtf8Spec = describe "lazyDecodeUtf8" $ do
  it "converts lazy Char8.ByteString to Text" $ do
    lazyDecodeUtf8 ("Some Text" :: L8.ByteString) `shouldBe` ("Some Text" :: Text)
    lazyDecodeUtf8 ("" :: L8.ByteString) `shouldBe` ("" :: Text)
