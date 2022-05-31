{-# LANGUAGE OverloadedStrings #-}

module API.Telegram.TypesSpec
  ( spec,
  )
where

import API.Telegram.Types as TG
  ( Error (Error),
    Response (ErrorResponse, ResponseWith),
    fromResponse,
  )
import Data.Aeson as Aeson (ToJSON (toJSON))
import Test.App.Error as App (isAPIError)
import Test.Hspec
  ( Spec,
    anyException,
    context,
    describe,
    it,
    shouldReturn,
    shouldThrow,
  )

spec :: Spec
spec = do
  fromResponseSpec

fromResponseSpec :: Spec
fromResponseSpec = describe "fromResponse" $ do
  context "when Telegram API responded with json" $
    it "extracts data of Aeson.ToJSON class from API.Telegram.Response" $ do
      fromResponse (ResponseWith $ Aeson.toJSON ([1, 2, 3, 4] :: [Integer]))
        `shouldReturn` ([1, 2, 3, 4] :: [Integer])
      fromResponse (ResponseWith $ Aeson.toJSON True)
        `shouldReturn` True
  context "when json can't be parsed to expected data type" $
    it "throws error" $ do
      (fromResponse (ResponseWith $ Aeson.toJSON True) :: IO Integer)
        `shouldThrow` App.isAPIError
      (fromResponse (ResponseWith $ Aeson.toJSON ['a', 'b']) :: IO Bool)
        `shouldThrow` App.isAPIError
  context "when Telegram API responded with error" $
    it "throws error" $ do
      (fromResponse (ErrorResponse $ TG.Error 101 "sommin bad happn :(") :: IO Bool)
        `shouldThrow` App.isAPIError
