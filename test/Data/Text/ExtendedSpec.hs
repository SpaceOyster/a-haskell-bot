{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ExtendedSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Extended as T (Text, lazyDecodeUtf8, pack, tshow)
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

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
  context "backslash sequences aren't tested" $
    prop "converts lazy Char8.ByteString to Text" $
      \someString -> do
        let someString' = filter (== '\\') someString
        let someByteString = L8.pack someString'
        let someText = T.pack someString'
        lazyDecodeUtf8 someByteString `shouldBe` someText
