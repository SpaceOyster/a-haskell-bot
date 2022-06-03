{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.ExtendedSpec
  ( spec,
  )
where

import Data.Char (isSpace)
import Data.Maybe (isJust)
import Network.URI.Extended
import Test.Hspec
import Test.QuickCheck

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
addPathSpec = do
  describe "addPath" $ do
    let uriString = "https://some.uri/path/here"
    let uriM = parseURI uriString
    it "appends string to URI path" $ do
      fmap (`addPath` "/whatever") uriM
        `shouldBe` parseURI (uriString <> "/whatever")
    it "does nothing to URI when second argument is empty string" $ do
      fmap (`addPath` "") uriM `shouldBe` uriM

addQueryParamsSpec :: Spec
addQueryParamsSpec = do
  describe "addQueryParams" $ do
    it "appends list of list of query parameters to URI" $ do
      let uriString = "https://some.uri/path/here"
      let uriM = parseURI uriString -- TODO fix this crap should be URI instead of (Maybe URI)
      fmap (`addQueryParams` []) uriM `shouldBe` uriM
      fmap
        ( `addQueryParams`
            [ "key" :=: "value",
              "key2" :=: "",
              "key3" :=: "value 3",
              "" :=: "value 4",
              "invalid key" :=: "value5"
            ]
        )
        uriM
        `shouldBe` parseURI (uriString <> "?key=value&key3=value%203")

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
