{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.BotReplies where

import Control.Monad.Trans (MonadTrans, lift)
import Data.Has
import qualified Data.Text.Extended as T (Text, replace, tshow)
import Effects.UsersDB (UserData (..))
import Prelude hiding (repeat)

data Replies = Replies
  { help :: T.Text,
    greeting :: T.Text,
    repeat :: T.Text,
    unknown :: T.Text,
    settingsSaved :: T.Text
  }
  deriving (Show)

instance Has Replies Replies where
  obtain = id

insertUserData :: UserData -> T.Text -> T.Text
insertUserData ud = T.replace "%n" (T.tshow $ getEchoMultiplier ud)

class (Monad m) => MonadBotReplies m where
  getReplies :: m Replies

getReply :: (MonadBotReplies m) => (Replies -> T.Text) -> m T.Text
getReply f = f <$> getReplies

instance
  {-# OVERLAPPABLE #-}
  (MonadBotReplies m, MonadTrans t, Monad (t m)) =>
  MonadBotReplies (t m)
  where
  getReplies = lift getReplies
