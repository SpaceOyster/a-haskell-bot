{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.ExtendedSpec
  ( spec,
  )
where

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
stringifyQueryListSpec = do
  describe "stringifyQueryList" $ do
    it "transforms [QueryParam] into correct query string" $ do
      let qparams =
            [ "key1" :=: "value 1",
              "key2" :=: "",
              "key3" :=: "value 3",
              "" :=: "value 4",
              "invalid key" :=: "value5"
            ]
      stringifyQueryList qparams `shouldBe` "key1=value%201&key3=value%203"
    it "transforms empty [QueryParam] into empty string" $ do
      stringifyQueryList [] `shouldBe` ""
    it "doesn't eliminate duplicate keys" $ do
      stringifyQueryList ["key" :=: "1", "key" :=: "2"]
        `shouldBe` "key=1&key=2"

stringifyQueryPairSpec :: Spec
stringifyQueryPairSpec = do
  describe "stringifyQueryPair" $ do
    context "Runs in context of MonadFail" $ do
      it "turns (key, Just value) pair into URI query string" $ do
        stringifyQueryPair ("key" :=: "value") `shouldReturn` "key=value"
      it "properly escapes characters of value" $ do
        stringifyQueryPair ("key" :=: "value with space")
          `shouldReturn` "key=value%20with%20space"
      it "fails when key string has unescaped characters" $ do
        stringifyQueryPair ("unescaped key" :=: "whatever") `shouldBe` Nothing
      it "fails when key is empty string" $ do
        stringifyQueryPair ("" :=: "whatever") `shouldBe` Nothing
      it "fails when value is empty string" $ do
        stringifyQueryPair ("whatever" :=: "") `shouldBe` Nothing
      it "fails when value is not present at all: (key, Nothing)" $ do
        stringifyQueryPair ("key" :=: "") `shouldBe` Nothing
