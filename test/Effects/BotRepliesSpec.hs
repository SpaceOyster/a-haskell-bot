{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Effects.BotRepliesSpec (spec) where

import Data.Functor.Identity (Identity (Identity))
import qualified Effects.BotReplies as BR
  ( MonadBotReplies (..),
    Replies (Replies, greeting, help, repeat, settingsSaved, unknown),
    getReply,
    insertUserData,
  )
import Effects.UsersDB (UserData (UserData))
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = describe "Effects.BotReplies" $ do
  insertUserDataSpec
  getReplySpec

insertUserDataSpec :: Spec
insertUserDataSpec =
  describe "insertUserData" $ do
    it "replaces `%n` subsequences in Text with Users Echo multiplier" $ do
      BR.insertUserData (UserData 3) "some text %n" `shouldBe` "some text 3"
      BR.insertUserData (UserData 42) "some text %n" `shouldBe` "some text 42"
      BR.insertUserData (UserData (-1)) "some text %n" `shouldBe` "some text -1"
      BR.insertUserData (UserData 0) "some text %n" `shouldBe` "some text 0"
      BR.insertUserData (UserData 17) "%n wha%ntever %n" `shouldBe` "17 wha17tever 17"
    it "doesn't change Text if `%n` doesn't occur." $ do
      BR.insertUserData (UserData 3) "some text" `shouldBe` "some text"

newtype TestBotRepliesMonad a = TestBotRepliesMonad {unTestBotRepliesMonad :: Identity a}
  deriving (Functor, Applicative, Monad, Show, Eq)

instance BR.MonadBotReplies TestBotRepliesMonad where
  getReplies =
    TestBotRepliesMonad . pure $
      BR.Replies
        { BR.help = "help text",
          BR.greeting = "greeting text",
          BR.repeat = "repeat text",
          BR.unknown = "unknown text",
          BR.settingsSaved = "settingsSaved text"
        }

getReplySpec :: Spec
getReplySpec = describe "getReply" $
  context "runs in context of MonadBotReplies" $ do
    it "gets on of reply messages from `Replies`" $
      do
        BR.getReply BR.help `shouldBe` TestBotRepliesMonad (pure "help text")
        BR.getReply BR.greeting `shouldBe` TestBotRepliesMonad (pure "greeting text")
        BR.getReply BR.repeat `shouldBe` TestBotRepliesMonad (pure "repeat text")
        BR.getReply BR.unknown `shouldBe` TestBotRepliesMonad (pure "unknown text")
        BR.getReply BR.settingsSaved `shouldBe` TestBotRepliesMonad (pure "settingsSaved text")
