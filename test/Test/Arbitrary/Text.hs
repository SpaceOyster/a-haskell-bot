module Test.Arbitrary.Text where

import Data.Text as T (Text, pack)
import Test.Arbitrary.String
  ( CleanString (getCleanString),
    DirtyString (getDirtyString),
    NonEmptyCleanString (getNonEmptyCleanString),
  )
import Test.QuickCheck (Arbitrary (arbitrary))

newtype AnyText = AnyText {getAnyText :: Text}
  deriving (Show)

instance Arbitrary AnyText where
  arbitrary = AnyText . T.pack <$> arbitrary

newtype CleanText = CleanText {getCleanText :: Text}
  deriving (Show)

instance Arbitrary CleanText where
  arbitrary = CleanText . T.pack . getCleanString <$> arbitrary

newtype NonEmptyCleanText = NonEmptyCleanText {getNonEmptyCleanText :: Text}
  deriving (Show)

instance Arbitrary NonEmptyCleanText where
  arbitrary = NonEmptyCleanText . T.pack . getNonEmptyCleanString <$> arbitrary

newtype DirtyText = DirtyText {getDirtyText :: Text}
  deriving (Show)

instance Arbitrary DirtyText where
  arbitrary = DirtyText . T.pack . getDirtyString <$> arbitrary
