{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Effects.UsersDBSpec (spec) where

import Effects.UsersDB
import Test.Hspec (Spec,describe, it, shouldBe, context, pending, shouldReturn)
import Data.Functor.Identity ( Identity(Identity) )
import Data.Hashable as H ( Hashable(hash), Hash )
import Data.Map as Map (Map(..), alter, insert, empty, lookup, fromList)
import Control.Monad.State (State, get, put, gets, evalState, when)

spec :: Spec
spec = describe "Effects.BotReplies" $ do
  getUserDataMSpec
  getUserMultiplierSpec
  getUserMultiplierMSpec
  setUserMultiplierSpec
  orDefaultDataSpec

instance Show UserData where
  show (UserData x) = "UserData { getEchoMultiplier = "<> show x <>" }"

instance Eq UserData where
  (UserData a) == (UserData b) = a == b

newtype TestUser = TestUser {unTestUser :: Integer}
  deriving (Show, Eq)

testUser :: TestUser
testUser = TestUser 12345

testMap :: Map Hash UserData
testMap = fromList [(1, UserData 12), (2,UserData 2), (1235, UserData 42)]

instance Hashable TestUser where
  hash = unTestUser

type TestDB = Map Integer UserData

newtype TestMonadUsersDB a = TestMonadUsersDB {unTestMonadUsersDB :: State TestDB a}
  deriving  (Functor, Applicative, Monad)

evalTest :: TestMonadUsersDB a -> TestDB -> a
evalTest m = evalState (unTestMonadUsersDB m)

instance MonadUsersDB TestMonadUsersDB where
  defaultUserData = pure $ UserData 1000
  getUserData user = TestMonadUsersDB $ gets (Map.lookup (hash user))
  setUserData user udata = TestMonadUsersDB $ do
    map <- get
    put $ Map.insert (H.hash user) udata map
  modifyUserData user morph = TestMonadUsersDB $ do
    map <- get
    put $ Map.alter (fmap morph) (hash user) map

getUserDataMSpec :: Spec
getUserDataMSpec = describe "getUserDataM" $ do
  let testMap = fromList [(1, UserData 12), (2,UserData 2), (1235, UserData 42)]
  let eval x = evalTest x testMap
  context "when got `Just user`" $ do
    it "gets `Just UserData` for user" $ do
      eval (getUserDataM (Just $ TestUser 1)) `shouldBe` Just (UserData 12)
      eval (getUserDataM (Just $ TestUser 2)) `shouldBe` Just (UserData 2)
      eval (getUserDataM (Just $ TestUser 1235)) `shouldBe` Just (UserData 42)
    it "gets Nothing if user is not present in DB" $ do
      eval (getUserDataM (Just $ TestUser 1111)) `shouldBe` Nothing
      eval (getUserDataM (Just $ TestUser 2222)) `shouldBe` Nothing
  context "when got (Nothing :: Maybe user)" $ do
    it "gets Nothing" $ do
      eval (getUserDataM (Nothing :: Maybe TestUser)) `shouldBe` Nothing

getUserMultiplierSpec :: Spec
getUserMultiplierSpec = describe "getUserMultiplier" $ do
  let testMap = fromList [(1, UserData 12), (2,UserData 2), (1235, UserData 42)]
  let eval x = evalTest x testMap
  context "when user is present in DB" $ 
    it "gets echo multiplier for user" $ do
      eval (getUserMultiplier (TestUser 1)) `shouldBe` 12
      eval (getUserMultiplier (TestUser 2)) `shouldBe` 2
      eval (getUserMultiplier (TestUser 1235)) `shouldBe` 42
  context "when user is not present in DB" $ 
    it "gets default echo multiplier" $ do
      eval (getUserMultiplier (TestUser 1111)) `shouldBe` 1000
      eval (getUserMultiplier (TestUser 2222)) `shouldBe` 1000

getUserMultiplierMSpec :: Spec
getUserMultiplierMSpec = describe "getUserMultiplierM" $ do
  let testMap = fromList [(1, UserData 12), (2,UserData 2), (1235, UserData 42)]
  let eval x = evalTest x testMap
  context "when got `Just user`" $ context "when user is present in DB" $
    it "gets echo multiplier for user" $ do
      eval (getUserMultiplierM (Just $ TestUser 1)) `shouldBe` 12
      eval (getUserMultiplierM (Just $ TestUser 2)) `shouldBe` 2
      eval (getUserMultiplierM (Just $ TestUser 1235)) `shouldBe` 42
  context "when got `Just user`" $ context "when user is not present in DB" $
    it "gets default echo multiplier" $ do
      eval (getUserMultiplierM (Just $ TestUser 1111)) `shouldBe` 1000
      eval (getUserMultiplierM (Just $ TestUser 2222)) `shouldBe` 1000
  context "when got (Nothing :: Maybe user)" $
    it "gets default echo multiplier" $ do
      eval (getUserMultiplierM (Nothing :: Maybe TestUser)) `shouldBe` 1000

setUserMultiplierSpec :: Spec
setUserMultiplierSpec = describe "setUserMultiplier" $ do
  let testMap = fromList [(1, UserData 12), (2,UserData 2), (1235, UserData 42)]
  let eval x = evalTest x testMap
  let check user n = setUserMultiplier user n >> getUserMultiplier user
  it "sets echo multiplier for user" $ do
    eval (check (TestUser 1) 100) `shouldBe` 100
    eval (check (TestUser 111) 100) `shouldBe` 100
    eval (check (TestUser 123) 123) `shouldBe` 123

orDefaultDataSpec :: Spec
orDefaultDataSpec = describe "orDefaultData" $ do
  let testMap = Map.empty
  let eval x = evalTest x testMap
  context "when got `m (Just UserData)`" $
    it "returns `m UserData`" $ do
      eval (orDefaultData (pure $ Just $ UserData 1)) `shouldBe` UserData 1
      eval (orDefaultData (pure $ Just $ UserData 12)) `shouldBe` UserData 12
      eval (orDefaultData (pure $ Just $ UserData 123)) `shouldBe` UserData 123
  context "when got `m (Nothing :: UserData)`" $
    it "returns `m defaultUserData`" $ 
      eval (orDefaultData (pure Nothing)) `shouldBe` UserData 1000
