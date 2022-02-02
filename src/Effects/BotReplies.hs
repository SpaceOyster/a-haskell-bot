module Effects.BotReplies where

import qualified Bot.Replies as Bot
import qualified Data.Text as T

class (Monad m) =>
      MonadBotReplies m
  where
  getReplies :: m Bot.Replies

getReply :: (MonadBotReplies m) => (Bot.Replies -> T.Text) -> m T.Text
getReply f = f <$> getReplies
