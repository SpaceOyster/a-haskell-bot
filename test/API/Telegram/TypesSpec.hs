{-# LANGUAGE OverloadedStrings #-}

module API.Telegram.TypesSpec
  ( spec,
  )
where

import API.Telegram.Types as TG
  ( CallbackQuery (..),
    Message (..),
    Response (ErrorResponse, ResponseWith),
    Update (..),
    User,
    fromResponse,
  )
import Data.Aeson as Aeson (ToJSON (toJSON))
import Test.App.Error as App (isAPIError)
import Test.Arbitrary.Telegram.Types ()
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldReturn,
    shouldThrow,
  )
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  fromResponseSpec

fromResponseSpec :: Spec
fromResponseSpec = describe "fromResponse" $ do
  context "when Telegram API responded with json" $
    prop "extracts data of Aeson.ToJSON class from API.Telegram.Response" $
      \(tgMsg, tgCB, tgUpd, tgUpds) -> do
        let tgMsgJSON = Aeson.toJSON (tgMsg :: TG.Message)
        let tgCBJSON = Aeson.toJSON (tgCB :: TG.CallbackQuery)
        let tgUpdJSON = Aeson.toJSON (tgUpd :: TG.Update)
        let tgUpdsJSON = Aeson.toJSON (tgUpds :: [TG.Update])
        fromResponse (ResponseWith tgMsgJSON) `shouldReturn` tgMsg
        fromResponse (ResponseWith tgCBJSON) `shouldReturn` tgCB
        fromResponse (ResponseWith tgUpdJSON) `shouldReturn` tgUpd
        fromResponse (ResponseWith tgUpdsJSON) `shouldReturn` tgUpds
  context "when json can't be parsed to expected data type" $
    prop "throws APIError" $ \(tgMsg, tgCB, tgUpd, tgUpds) -> do
      let tgMsgJSON = Aeson.toJSON (tgMsg :: TG.Message)
      let tgCBJSON = Aeson.toJSON (tgCB :: TG.CallbackQuery)
      let tgUpdJSON = Aeson.toJSON (tgUpd :: TG.Update)
      let tgUpdsJSON = Aeson.toJSON (tgUpds :: [TG.Update])
      (fromResponse $ ResponseWith tgMsgJSON :: IO TG.User)
        `shouldThrow` App.isAPIError
      (fromResponse $ ResponseWith tgCBJSON :: IO TG.User)
        `shouldThrow` App.isAPIError
      (fromResponse $ ResponseWith tgUpdJSON :: IO TG.User)
        `shouldThrow` App.isAPIError
      (fromResponse $ ResponseWith tgUpdsJSON :: IO TG.User)
        `shouldThrow` App.isAPIError
  context "when Telegram API responded with error" $
    prop "throws APIError" $ \tgError ->
      (fromResponse $ ErrorResponse tgError :: IO Bool) `shouldThrow` App.isAPIError
