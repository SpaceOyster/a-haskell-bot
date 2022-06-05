{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.ExtendedSpec
  ( spec,
  )
where

import Data.Char (isSpace)
import Network.URI.Extended
  ( QueryParam (..),
    URI (..),
    URIAuth (..),
    addPath,
    addQueryParams,
    isAllowedInURI,
    isUnescapedInURI,
    isUnescapedInURIComponent,
    stringifyQueryList,
    stringifyQueryPair,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
  )
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    NonEmptyList (NonEmpty),
    Testable (property),
    chooseInteger,
    elements,
    listOf,
    listOf1,
    suchThat,
    (==>),
  )

spec :: Spec
spec = do
  stringifyQueryPairSpec
  stringifyQueryListSpec
  addQueryParamsSpec
  addPathSpec

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

instance Arbitrary QueryParam where
  arbitrary = do
    key <- getNonEmptyCleanString <$> arbitrary
    value <- getNonEmptyCleanString <$> arbitrary
    pure (key :=: value)

instance Arbitrary URI where
  arbitrary = do
    uriScheme <- (<> ":") . getCleanString <$> arbitrary
    uriAuthority <- arbitrary
    uriPath <- ('/' :) . getCleanString <$> arbitrary
    uriQuery <- ('?' :) . getCleanString <$> arbitrary
    uriFragment <- ('#' :) . getCleanString <$> arbitrary
    pure $
      URI
        { uriScheme,
          uriAuthority,
          uriPath,
          uriQuery,
          uriFragment
        }

instance Arbitrary URIAuth where
  arbitrary = do
    uriUserInfo <- ("//" <>) . (<> "@") . getCleanString <$> arbitrary
    uriRegName <- getCleanString <$> arbitrary
    uriPort <- (':' :) . show <$> chooseInteger (0, 65535)
    pure $ URIAuth {uriUserInfo, uriRegName, uriPort}

addPathSpec :: Spec
addPathSpec =
  describe "addPath" $ do
    it "appends string to URI path" $ do
      property $ \(uri, str) ->
        addPath uri str `shouldBe` uri {uriPath = uriPath uri <> str}
    it "does nothing to URI when second argument is empty string" $ do
      property $ \uri -> uri `addPath` "" `shouldBe` uri

addQueryParamsSpec :: Spec
addQueryParamsSpec = describe "addQueryParams" $ do
  it "appends list of list of query parameters to URI" $ do
    property $ \(uri, NonEmpty qps) -> do
      let prefix = if null (uriQuery uri) then '?' else '&'
      addQueryParams uri qps `shouldBe` uri {uriQuery = uriQuery uri <> (prefix : stringifyQueryList qps)}
  it "doesn't change uri if query parameters list is empty" $ do
    property $ \uri -> do
      addQueryParams uri [] `shouldBe` uri

stringifyQueryListSpec :: Spec
stringifyQueryListSpec = describe "stringifyQueryList" $ do
  it "transforms [QueryParam] into correct query string" $ do
    property $ \(NonEmpty qparams) ->
      stringifyQueryList qparams `shouldSatisfy` all isUnescapedInURI
  it "transforms empty [QueryParam] into empty string" $ do
    stringifyQueryList [] `shouldBe` ""
  it "doesn't eliminate duplicate keys" $
    property $ \(key1 :=: value1) ->
      stringifyQueryList [key1 :=: value1, key1 :=: value1]
        `shouldNotBe` stringifyQueryList [key1 :=: value1]

notEmpty :: [a] -> Bool
notEmpty = not . null

notOnlySpaceChars :: [Char] -> Bool
notOnlySpaceChars = not . all isSpace

stringifyQueryPairSpec :: Spec
stringifyQueryPairSpec =
  describe "stringifyQueryPair" $ context "Runs in context of MonadFail" $ do
    it "turns `key :=: value` pair into URI query string" $ do
      property $ \(key :=: value) ->
        notEmpty key && notOnlySpaceChars value ==> do
          stringifyQueryPair (key :=: value)
            `shouldSatisfy` maybe True (all isUnescapedInURI)
    it "properly escapes characters of value" $ do
      property $ \(key, DirtyString value) -> do
        (stringifyQueryPair (key :=: value) :: Maybe String)
          `shouldSatisfy` maybe True (all isAllowedInURI)
    it "fails when key string has characters other than alphanumeric or one of \"-_.~\"" $ do
      property $ \(DirtyString key, value) -> do
        stringifyQueryPair (key :=: value) `shouldBe` Nothing
    it "fails when key is empty string" $ do
      property $ \value ->
        stringifyQueryPair ("" :=: value) `shouldBe` Nothing
    it "fails when value is empty string" $ do
      property $ \key ->
        stringifyQueryPair (key :=: "") `shouldBe` Nothing
