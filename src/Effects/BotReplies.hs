module Effects.BotReplies where

import qualified Bot.Replies as Bot
import qualified Data.Text as T

class (Monad m) =>
      MonadBotReplies m
  where
  getReplies :: m Bot.Replies
  getReply :: (Bot.Replies -> T.Text) -> m T.Text
