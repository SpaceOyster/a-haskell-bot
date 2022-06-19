{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary.JSON where

import Data.Aeson as Aeson (KeyValue ((.=)), Value, object)
import Test.Arbitrary.Text (CleanText (getCleanText))
import Test.QuickCheck (Arbitrary (arbitrary))

newtype JSONObject = JSONObject {getJSONObject :: Aeson.Value}
  deriving (Show)

instance Arbitrary JSONObject where
  arbitrary = do
    key <- getCleanText <$> arbitrary
    value <- getCleanText <$> arbitrary
    pure $ JSONObject $ object [key .= value]
