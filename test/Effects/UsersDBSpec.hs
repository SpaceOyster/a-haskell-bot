{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effects.UsersDBSpec (spec) where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    StateT (StateT),
    evalState,
    gets,
  )
import Data.Hashable as H (Hashable (..))
import Data.Map as Map
  ( Map,
    alter,
    delete,
    empty,
    insert,
    lookup,
  )
import Effects.UsersDB
  ( MonadUsersDB (..),
    UserData (..),
    getUserDataM,
    getUserMultiplier,
    getUserMultiplierM,
    orDefaultData,
    setUserMultiplier,
  )
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Testable (property))

spec :: Spec
spec = do
  getUserDataMSpec
  getUserMultiplierSpec
  getUserMultiplierMSpec
  setUserMultiplierSpec
  orDefaultDataSpec

newtype TestUser = TestUser {unTestUser :: Integer}
  deriving (Show, Eq)

instance Hashable TestUser where
  hash = unTestUser

instance Arbitrary TestUser where
  arbitrary = TestUser <$> arbitrary

instance Arbitrary UserData where
  arbitrary = UserData <$> arbitrary

type TestDB = Map Integer UserData

newtype TestMonadUsersDB a = TestMonadUsersDB {unTestMonadUsersDB :: State TestDB a}
  deriving (Functor, Applicative, Monad)

evalTest :: TestMonadUsersDB a -> TestDB -> a
evalTest m = evalState (unTestMonadUsersDB m)

instance MonadUsersDB TestMonadUsersDB where
  defaultUserData = pure $ UserData 1000
  getUserData user = TestMonadUsersDB $ gets (Map.lookup (hash user))
  setUserData user udata = TestMonadUsersDB $ do
    map' <- get
    put $ Map.insert (H.hash user) udata map'
  modifyUserData user morph = TestMonadUsersDB $ do
    map' <- get
    put $ Map.alter (fmap morph) (hash user) map'

getUserDataMSpec :: Spec
getUserDataMSpec = describe "getUserDataM" $ do
  context "when got `Just user`" $ do
    it "gets `Just UserData` for user" $
      property $ \(user, usersMap, userData) -> do
        let usersMap' = Map.insert (hash user) userData usersMap
        evalTest (getUserDataM $ Just (user :: TestUser)) usersMap' `shouldBe` Just userData
    it "gets Nothing if user is not present in DB" $
      property $ \user ->
        evalTest (getUserDataM $ Just (user :: TestUser)) Map.empty `shouldBe` Nothing
  context "when got (Nothing :: Maybe user)" $
    it "gets Nothing" $
      property $ \usersMap ->
        evalTest (getUserDataM (Nothing :: Maybe TestUser)) usersMap `shouldBe` Nothing

getUserMultiplierSpec :: Spec
getUserMultiplierSpec = describe "getUserMultiplier" $ do
  context "when user is present in DB" $
    it "gets echo multiplier for user" $
      property $ \(user, usersMap, userData) -> do
        let usersMap' = Map.insert (hash user) userData usersMap
        evalTest (getUserMultiplier (user :: TestUser)) usersMap' `shouldBe` getEchoMultiplier userData
  context "when user is not present in DB" $
    it "gets default echo multiplier" $
      property $ \(user, usersMap) -> do
        let usersMap' = Map.delete (hash user) usersMap
        evalTest (getUserMultiplier (user :: TestUser)) usersMap'
          `shouldBe` evalTest (getEchoMultiplier <$> defaultUserData) usersMap'

getUserMultiplierMSpec :: Spec
getUserMultiplierMSpec = describe "getUserMultiplierM" $ do
  context "when got `Just user`" $ do
    context "when user is present in DB" $
      it "gets echo multiplier for user" $
        property $ \(user, usersMap, userData) -> do
          let usersMap' = Map.insert (hash user) userData usersMap
          evalTest (getUserMultiplierM $ Just (user :: TestUser)) usersMap' `shouldBe` getEchoMultiplier userData
    context "when user is not present in DB" $
      it "gets default echo multiplier" $
        property $ \(user, usersMap) -> do
          let usersMap' = Map.delete (hash user) usersMap
          evalTest (getUserMultiplierM $ Just (user :: TestUser)) usersMap'
            `shouldBe` evalTest (getEchoMultiplier <$> defaultUserData) usersMap'
  context "when got (Nothing :: Maybe user)" $
    it "gets default echo multiplier" $
      property $ \usersMap ->
        evalTest (getUserMultiplierM (Nothing :: Maybe TestUser)) usersMap
          `shouldBe` evalTest (getEchoMultiplier <$> defaultUserData) usersMap

setUserMultiplierSpec :: Spec
setUserMultiplierSpec = describe "setUserMultiplier" $ do
  let check user n = setUserMultiplier user n >> getUserMultiplier user
  it "sets echo multiplier for user" $
    property $ \(user, usersMap, multiplier) ->
      evalTest (check (user :: TestUser) multiplier) usersMap `shouldBe` multiplier

orDefaultDataSpec :: Spec
orDefaultDataSpec = describe "orDefaultData" $ do
  context "when got `m (Just UserData)`" $
    it "returns `m UserData`" $
      property $ \(user, usersMap, userData) -> do
        let usersMap' = Map.insert (hash (user :: TestUser)) userData usersMap
        evalTest (orDefaultData (pure $ Just userData)) usersMap'
          `shouldBe` evalTest (pure userData) usersMap'
  context "when got `m (Nothing :: UserData)`" $
    it "returns `m defaultUserData`" $
      evalTest (orDefaultData (pure Nothing)) Map.empty
        `shouldBe` evalTest defaultUserData Map.empty
