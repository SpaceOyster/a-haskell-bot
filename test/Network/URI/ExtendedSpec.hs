module Network.URI.ExtendedSpec
  ( spec
  ) where

import Network.URI.Extended
import Test.Hspec

spec :: Spec
spec = do
  describe "Network.URI.Extended" $ do
    stringifyQueryPairSpec
    stringifyQueryListSpec
    addQueryParamsSpec
    addPathSpec

stringifyQueryPairSpec :: Spec
stringifyQueryPairSpec = do
  describe "stringifyQueryPair" $ do
    it "turns (key, Just value) pair into URI query string inside MonadFail" $ do
      (stringifyQueryPair ("key", Just "value") :: [String]) `shouldBe`
        ["key=value"]
      (stringifyQueryPair ("key", Just "value with space") :: Maybe String) `shouldBe`
        Just "key=value%20with%20space"
    context "when fails empty string is returned" $ do
      it "fails when key string has unescaped characters" $ do
        (stringifyQueryPair ("unescaped key", Just "whatever") :: [String]) `shouldBe`
          []
      it "fails when key is empty string" $ do
        (stringifyQueryPair ("", Just "whatever") :: [String]) `shouldBe` []
      it "fails when value is empty string" $ do
        (stringifyQueryPair ("whatever", Just "") :: [String]) `shouldBe` []
      it "fails when value is not present at all: (key, Nothing)" $ do
        (stringifyQueryPair ("key", Nothing) :: [String]) `shouldBe` []

stringifyQueryListSpec :: Spec
stringifyQueryListSpec = do
  describe "stringifyQueryList" $ do
    it "transforms [QueryParam] into correct query string" $ do
      stringifyQueryList
        [("key1", Just "value 1"), ("key2", Nothing), ("key3", Just "value 3")] `shouldBe`
        "key1=value%201&key3=value%203"
    it "transforms empty [QueryParam] into empty string" $ do
      stringifyQueryList [] `shouldBe` ""
    it "doesn't eliminate duplicate keys" $ do
      stringifyQueryList [("key", Just "1"), ("key", Just "2")] `shouldBe`
        "key=1&key=2"

addQueryParamsSpec :: Spec
addQueryParamsSpec = do
  describe "addQueryParams" $ do
    it "appends list of list of query parameters to URI" $ do
      let uriString = "https://some.uri/path/here"
      let uriM = parseURI uriString
      fmap (`addQueryParams` []) uriM `shouldBe` uriM
      fmap
        (`addQueryParams` [ ("key", Just "value")
                          , ("key2", Nothing)
                          , ("invalid key", Just "some")
                          ])
        uriM `shouldBe`
        parseURI (uriString <> "?key=value")

addPathSpec :: Spec
addPathSpec = do
  describe "addPath" $ do
    let uriString = "https://some.uri/path/here"
    let uriM = parseURI uriString
    it "appends string to URI path" $ do
      fmap (`addPath` "/whatever") uriM `shouldBe`
        parseURI (uriString <> "/whatever")
    it "does nothing to URI when second argument is empty string" $ do
      fmap (`addPath` "") uriM `shouldBe` uriM
