{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Bot where

import Bot.Replies as Bot
import Control.Monad (forever, join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.State (StateT, evalStateT, lift)
import Control.Monad.Trans (MonadTrans (..))
import Data.Function ((&))
import qualified Data.Hashable as H
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP (MonadHTTP (..))
import qualified Effects.Log as Log (MonadLog, logInfo)
import qualified Effects.UsersDB as DB

repeatPrompt ::
  (H.Hashable u, MonadThrow m, DB.MonadUsersDB m, BR.MonadBotReplies m) =>
  Maybe u ->
  m T.Text
repeatPrompt userM = do
  userData <- userM & DB.getUserDataM & DB.orDefaultData
  prompt <- BR.getReply Bot.repeat
  pure $ Bot.insertUserData userData prompt

data BotDSL u ret
  = FetchUpdates ([u] -> BotDSL u ret)
  | ReactToUpdates [u] (BotDSL u ret)
  | Done ret

-- | command has to be between 1-32 chars long
-- description has to be between 3-256 chars long
data BotCommand
  = Start
  | Help
  | Repeat
  | UnknownCommand
  deriving (Show, Enum, Bounded)

describe :: BotCommand -> T.Text
describe Start = "Greet User"
describe Help = "Show help text"
describe Repeat = "Set echo multiplier"
describe UnknownCommand = "Unknown Command"

parseCommand :: T.Text -> BotCommand
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
  ( MonadThrow m,
    HTTP.MonadHTTP m,
    Log.MonadLog m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m,
    StatefulBotMonad st,
    Monad (st m)
  ) =>
  Int ->
  st m a
loop period = interpret botLoop

class (MonadTrans st) => StatefulBotMonad st where
  type Update st
  type Response st
  type Message st
  data Entity st
  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    st m [Update st]
  doBotThing ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m,
      Monad (st m)
    ) =>
    st m [Response st]
  doBotThing = fetchUpdates >>= reactToUpdates
  qualifyUpdate :: (MonadThrow m) => Update st -> m (Entity st)
  reactToUpdate ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Update st ->
    st m [Response st]
  reactToUpdates ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m,
      Monad (st m)
    ) =>
    [Update st] ->
    st m [Response st]
  reactToUpdates updates = do
    lift $ Log.logInfo "processing each update"
    join <$> mapM reactToUpdate updates
  execCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    BotCommand ->
    (Message st -> st m (Response st))
  interpret ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      BR.MonadBotReplies m,
      DB.MonadUsersDB m,
      Monad (st m)
    ) =>
    BotDSL (Update st) a ->
    st m a
  interpret (FetchUpdates next) = fetchUpdates >>= interpret . next
  interpret (ReactToUpdates us next) =
    reactToUpdates us >> interpret next
  interpret (Done ret) = pure ret

botLoop :: BotDSL a ret
botLoop = FetchUpdates (\us -> ReactToUpdates us botLoop)

andThen :: BotDSL a r -> (r -> BotDSL a r') -> BotDSL a r'
andThen (Done ret) mkProgram = mkProgram ret
andThen (FetchUpdates next) mkProgram =
  FetchUpdates $ \updates -> next updates `andThen` mkProgram
andThen (ReactToUpdates us next) mkProgram =
  ReactToUpdates us (next `andThen` mkProgram)
