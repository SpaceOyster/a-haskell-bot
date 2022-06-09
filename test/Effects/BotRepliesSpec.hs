{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Effects.BotRepliesSpec (spec) where

import Data.Functor.Identity (Identity (Identity))
import Data.Text.Extended as T (intercalate, pack, tshow)
import qualified Effects.BotReplies as BR
  ( MonadBotReplies (..),
    Replies (Replies, greeting, help, repeat, settingsSaved, unknown),
    getReply,
    insertUserData,
  )
import Effects.UsersDB (UserData (UserData))
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "Effects.BotReplies" $ do
  insertUserDataSpec
  getReplySpec

insertUserDataSpec :: Spec
insertUserDataSpec = do
  prop "replaces `%n` subsequences in Text with Users Echo multiplier" $
    \(multiplier, stringSegments) -> do
      let textSegments = T.pack . filter (== '%') <$> (stringSegments :: [String])
      let str = T.intercalate "%n" textSegments
      let expexted = T.intercalate (T.tshow multiplier) textSegments
      BR.insertUserData (UserData multiplier) str `shouldBe` expexted
  prop "doesn't change Text if `%n` doesn't occur." $
    \(multiplier, someString) -> do
      let someText = T.pack $ filter (== '%') someString
      BR.insertUserData (UserData multiplier) someText `shouldBe` someText

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
