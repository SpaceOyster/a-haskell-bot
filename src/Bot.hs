{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot where

import Bot.Replies as Bot
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, evalStateT, lift)
import Data.Function ((&))
import qualified Data.Hashable as H
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP (MonadHTTP(..))
import qualified Effects.Log as Log (MonadLog, logInfo)
import qualified Effects.UsersDB as DB

repeatPrompt ::
     (H.Hashable u, MonadThrow m, DB.MonadUsersDB m, BR.MonadBotReplies m)
  => Maybe u
  -> m T.Text
repeatPrompt userM = do
  userData <- userM & DB.getUserDataM & DB.orDefaultData
  prompt <- BR.getReply Bot.repeat
  pure $ Bot.insertUserData userData prompt

-- | command has to be between 1-32 chars long
-- description has to be between 3-256 chars long
data Command
  = Start
  | Help
  | Repeat
  | UnknownCommand
  deriving (Show, Enum, Bounded)

describe :: Command -> T.Text
describe Start = "Greet User"
describe Help = "Show help text"
describe Repeat = "Set echo multiplier"
describe UnknownCommand = "Unknown Command"

parseCommand :: T.Text -> Command
parseCommand s =
  case T.toLower s of
    "start" -> Start
    "help" -> Help
    "repeat" -> Repeat
    _ -> UnknownCommand

isCommand :: T.Text -> Bool
isCommand "" = False
isCommand s = (== '/') . T.head $ s

loop ::
     ( MonadIO m
     , MonadThrow m
     , HTTP.MonadHTTP m
     , Log.MonadLog m
     , DB.MonadUsersDB m
     , BR.MonadBotReplies m
     , StatefulBotMonad st
     )
  => Int
  -> st
  -> m ()
loop period st =
  flip evalStateT st $ do
    _ <- forever $ Bot.doBotThing >> liftIO (threadDelay period)
    pure ()

class (Monoid st) =>
      StatefulBotMonad st
  where
  type Update st
  fetchUpdates ::
       (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m)
    => StateT st m [Update st]
  doBotThing ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => StateT st m [Response st]
  doBotThing = fetchUpdates >>= reactToUpdates
  data Entity st
  type Response st
  qualifyUpdate :: (MonadThrow m) => Update st -> m (Entity st)
  reactToUpdate ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => Update st
    -> StateT st m [Response st]
  reactToUpdates ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => [Update st]
    -> StateT st m [Response st]
  reactToUpdates updates = do
    lift $ Log.logInfo "processing each update"
    join <$> mapM reactToUpdate updates
  type Message st
  execCommand ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => Command
    -> (Message st -> StateT st m (Response st))
