{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.ExtendedSpec
  ( spec,
  )
where

import Data.Char (isSpace)
import Network.URI.Extended
  ( QueryParam (..),
    URI (..),
    addPath,
    addQueryParams,
    isAllowedInURI,
    isUnescapedInURI,
    stringifyQueryList,
    stringifyQueryPair,
  )
import Test.Arbitrary.String
  ( DirtyString (DirtyString),
  )
import Test.Arbitrary.URI ()
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( NonEmptyList (NonEmpty),
    (==>),
  )

spec :: Spec
spec = do
  stringifyQueryPairSpec
  stringifyQueryListSpec
  addQueryParamsSpec
  addPathSpec

addPathSpec :: Spec
addPathSpec =
  describe "addPath" $ do
    prop "appends string to URI path" $ \(uri, str) ->
      addPath uri str `shouldBe` uri {uriPath = uriPath uri <> str}
    prop "does nothing to URI when second argument is empty string" $
      \uri -> uri `addPath` "" `shouldBe` uri

addQueryParamsSpec :: Spec
addQueryParamsSpec = describe "addQueryParams" $ do
  prop "appends list of list of query parameters to URI" $
    \(uri, NonEmpty qps) -> do
      let prefix = if null (uriQuery uri) then '?' else '&'
      addQueryParams uri qps `shouldBe` uri {uriQuery = uriQuery uri <> (prefix : stringifyQueryList qps)}
  prop "doesn't change uri if query parameters list is empty" $
    \uri -> do
      addQueryParams uri [] `shouldBe` uri

stringifyQueryListSpec :: Spec
stringifyQueryListSpec = describe "stringifyQueryList" $ do
  prop "transforms [QueryParam] into correct query string" $
    \(NonEmpty qparams) ->
      stringifyQueryList qparams `shouldSatisfy` all isUnescapedInURI
  prop "transforms empty [QueryParam] into empty string" $
    stringifyQueryList [] `shouldBe` ""
  prop "doesn't eliminate duplicate keys" $
    \(key1 :=: value1) ->
      stringifyQueryList [key1 :=: value1, key1 :=: value1]
        `shouldNotBe` stringifyQueryList [key1 :=: value1]

notEmpty :: [a] -> Bool
notEmpty = not . null

notOnlySpaceChars :: [Char] -> Bool
notOnlySpaceChars = not . all isSpace

stringifyQueryPairSpec :: Spec
stringifyQueryPairSpec =
  describe "stringifyQueryPair" $ context "Runs in context of MonadFail" $ do
    prop "turns `key :=: value` pair into URI query string" $
      \(key :=: value) ->
        notEmpty key
          && notOnlySpaceChars value
          ==> stringifyQueryPair (key :=: value)
          `shouldSatisfy` maybe True (all isUnescapedInURI)
    prop "properly escapes characters of value" $
      \(key, DirtyString value) ->
        (stringifyQueryPair (key :=: value) :: Maybe String)
          `shouldSatisfy` maybe True (all isAllowedInURI)
    prop "fails when key string has characters other than alphanumeric or one of \"-_.~\"" $
      \(DirtyString key, value) ->
        stringifyQueryPair (key :=: value) `shouldBe` Nothing
    prop "fails when key is empty string" $
      \value ->
        stringifyQueryPair ("" :=: value) `shouldBe` Nothing
    prop "fails when value is empty string" $
      \key ->
        stringifyQueryPair (key :=: "") `shouldBe` Nothing
