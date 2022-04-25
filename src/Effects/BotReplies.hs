{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.BotReplies where

import qualified Bot.Replies as Bot
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Text as T

class (Monad m) => MonadBotReplies m where
  getReplies :: m Bot.Replies

getReply :: (MonadBotReplies m) => (Bot.Replies -> T.Text) -> m T.Text
getReply f = f <$> getReplies

instance
  {-# OVERLAPPABLE #-}
  (MonadBotReplies m, MonadTrans t, Monad (t m)) =>
  MonadBotReplies (t m)
  where
  getReplies = lift getReplies
