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

data BotDSL api ret
  = FetchUpdates ([Update api] -> BotDSL api ret)
  | ReactToUpdates [Update api] (BotDSL api ret)
  | QualifyUpdate (Update api) (Entity api -> BotDSL api ret)
  | ReactToMessage (Message api) ([Response api] -> BotDSL api ret)
  | ReactToCommand (Command api) ([Response api] -> BotDSL api ret)
  | ReactToCallback (CallbackQuery api) ([Response api] -> BotDSL api ret)
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

data Entity api
  = EMessage (Message api)
  | ECommand (Command api)
  | ECallback (CallbackQuery api)

class (MonadTrans st) => StatefulBotMonad st where
  type Update st
  type Response st
  type Message st
  type Command st
  type CallbackQuery st

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
      BR.MonadBotReplies m,
      Monad (st m)
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
  reactToCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Command st ->
    st m [Response st]
  reactToMessage ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    Message st ->
    st m [Response st]
  reactToCallback ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m, BR.MonadBotReplies m) =>
    CallbackQuery st ->
    st m [Response st]
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
    BotDSL st a ->
    st m a
  interpret (FetchUpdates next) = fetchUpdates >>= interpret . next
  interpret (ReactToUpdates us next) =
    reactToUpdates us >> interpret next
  interpret (QualifyUpdate u next) = lift (qualifyUpdate u) >>= interpret . next
  interpret (ReactToMessage m next) = reactToMessage m >>= interpret . next
  interpret (ReactToCommand c next) = reactToCommand c >>= interpret . next
  interpret (ReactToCallback c next) = reactToCallback c >>= interpret . next
  interpret (Done ret) = pure ret

botLoop :: BotDSL a ret
botLoop = FetchUpdates (\us -> ReactToUpdates us botLoop)

andThen :: BotDSL a r -> (r -> BotDSL a r') -> BotDSL a r'
andThen (Done ret) mkProgram = mkProgram ret
andThen (FetchUpdates next) mkProgram =
  FetchUpdates $ \updates -> next updates `andThen` mkProgram
andThen (ReactToUpdates us next) mkProgram =
  ReactToUpdates us (next `andThen` mkProgram)
andThen (QualifyUpdate u next) mkProgram = QualifyUpdate u $ \x -> next x `andThen` mkProgram
andThen (ReactToMessage m next) mkProgram = ReactToMessage m $ \x -> next x `andThen` mkProgram
andThen (ReactToCommand c next) mkProgram = ReactToCommand c $ \x -> next x `andThen` mkProgram
andThen (ReactToCallback c next) mkProgram = ReactToCallback c $ \x -> next x `andThen` mkProgram
