module Test.Arbitrary.Text where

import Data.Text as T (Text, pack)
import Test.Arbitrary.String
  ( CleanString (getCleanString),
    DirtyString (getDirtyString),
    NonEmptyCleanString (getNonEmptyCleanString),
  )
import Test.QuickCheck (Arbitrary (arbitrary), NonEmptyList (getNonEmpty))

newtype AnyText = AnyText {getAnyText :: Text}
  deriving (Show)

instance Arbitrary AnyText where
  arbitrary = AnyText . T.pack <$> arbitrary

newtype NonEmptyText = NonEmptyText {getNonEmptyText :: Text}
  deriving (Show)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack . getNonEmpty <$> arbitrary

newtype CleanText = CleanText {getCleanText :: Text}
  deriving (Show)

instance Arbitrary CleanText where
  arbitrary = CleanText . T.pack . getCleanString <$> arbitrary

newtype ShortCleanText = ShortCleanText {getShortCleanText :: Text}
  deriving (Show)

instance Arbitrary ShortCleanText where
  arbitrary = ShortCleanText . T.pack . getCleanString <$> arbitrary

newtype NonEmptyCleanText = NonEmptyCleanText {getNonEmptyCleanText :: Text}
  deriving (Show)

instance Arbitrary NonEmptyCleanText where
  arbitrary = NonEmptyCleanText . T.pack . getNonEmptyCleanString <$> arbitrary

newtype DirtyText = DirtyText {getDirtyText :: Text}
  deriving (Show)

instance Arbitrary DirtyText where
  arbitrary = DirtyText . T.pack . getDirtyString <$> arbitrary
