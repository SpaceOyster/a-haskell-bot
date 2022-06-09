module Test.Arbitrary.String where

import Network.URI (isUnescapedInURIComponent)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    elements,
    listOf,
    listOf1,
    suchThat,
  )

alphaChars :: [Char]
alphaChars = ['a' .. 'z'] <> ['A' .. 'Z']

numChars :: [Char]
numChars = ['0' .. '9']

alphaNumChars :: [Char]
alphaNumChars = alphaChars <> numChars

unreservedURIChars :: [Char]
unreservedURIChars = alphaNumChars <> "-_.~"

newtype CleanString = CleanString {getCleanString :: String}
  deriving (Show)

instance Arbitrary CleanString where
  arbitrary = do
    let allowedChars = unreservedURIChars
    CleanString <$> listOf (elements allowedChars)

newtype NonEmptyCleanString = NonEmptyCleanString {getNonEmptyCleanString :: String}
  deriving (Show)

instance Arbitrary NonEmptyCleanString where
  arbitrary = do
    let allowedChars = unreservedURIChars
    NonEmptyCleanString <$> listOf1 (elements allowedChars)

newtype DirtyString = DirtyString {getDirtyString :: String}
  deriving (Show)

instance Arbitrary DirtyString where
  arbitrary = DirtyString <$> arbitrary `suchThat` (not . all isUnescapedInURIComponent)
