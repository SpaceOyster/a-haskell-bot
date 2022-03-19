{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Bot where

import Bot.Replies as Bot
import App.Error as Ex (BotException(..))
import Control.Monad (ap, forM_, forever, join, liftM, (>=>))
import Control.Monad.Catch (MonadThrow (..))
import Data.Function ((&))
import qualified Data.Hashable as H
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
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
  | QualifyUpdate (Update api) (Entity api -> BotDSL api ret)
  | ReactToCommand (Command api) ([Response api] -> BotDSL api ret)
  | ReactToCallback (CallbackQuery api) ([Response api] -> BotDSL api ret)
  | GetAuthorsSettings (Message api) (DB.UserData -> BotDSL api ret)
  | EchoMessageNTimes (Message api) Int ([Response api] -> BotDSL api ret)
  | Done ret

instance Functor (BotDSL a) where
  fmap = liftM

instance Applicative (BotDSL a) where
  pure = return
  (<*>) = ap

instance Monad (BotDSL a) where
  return = Done
  t >>= mk =
    case t of
      Done ret -> mk ret
      FetchUpdates next -> FetchUpdates $ next >=> mk
      QualifyUpdate u next -> QualifyUpdate u $ next >=> mk
      ReactToCommand c next -> ReactToCommand c $ next >=> mk
      ReactToCallback c next -> ReactToCallback c $ next >=> mk
      GetAuthorsSettings m next -> GetAuthorsSettings m $ next >=> mk
      EchoMessageNTimes m n next -> EchoMessageNTimes m n $ next >=> mk

andThen :: BotDSL a r -> (r -> BotDSL a r') -> BotDSL a r'
andThen = (>>=)

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

parseCommand :: (MonadThrow m) => T.Text -> m BotCommand
parseCommand s =
  case T.toLower s of
    "start" -> pure Start
    "help" ->  pure Help
    "repeat" ->  pure Repeat
    _ -> throwM $ Ex.Ex $ "Unknown Command: " <> show s

isCommand :: T.Text -> Bool
isCommand "" = False
isCommand s = (== '/') . T.head $ s

loop :: EchoBotMonad m => Int -> m a
loop period = interpret botLoop

data Entity api
  = EMessage (Message api)
  | ECommand (Command api)
  | ECallback (CallbackQuery api)

class (Monad m) => EchoBotMonad m where
  type Update m
  type Response m
  type Message m
  type Command m
  type CallbackQuery m

  fetchUpdates :: m [Update m]
  qualifyUpdate :: Update m -> m (Entity m)
  reactToCommand :: Command m -> m [Response m]
  reactToCallback :: CallbackQuery m -> m [Response m]
  execCommand :: BotCommand -> (Message m -> m (Response m))
  getAuthorsSettings :: Message m -> m DB.UserData
  echoMessageNTimes :: Message m -> Int -> m [Response m]

reactToMessage :: EchoBotMonad m => Message m -> m [Response m]
reactToMessage msg = do
  settings <- getAuthorsSettings msg
  let n = DB.getEchoMultiplier settings
  Bot.echoMessageNTimes msg n

reactToUpdate :: EchoBotMonad m => Update m -> m [Response m]
reactToUpdate update = do
  qu <- qualifyUpdate update
  case qu of
    Bot.ECommand msg -> Bot.reactToCommand msg
    Bot.EMessage msg -> Bot.reactToMessage msg
    Bot.ECallback cq -> Bot.reactToCallback cq

reactToUpdates :: EchoBotMonad m => [Update m] -> m [Response m]
reactToUpdates updates = join <$> mapM reactToUpdate updates

doBotThing :: EchoBotMonad m => m [Response m]
doBotThing = forever (fetchUpdates >>= reactToUpdates)

interpret :: EchoBotMonad m => BotDSL m a -> m a
interpret (FetchUpdates next) = fetchUpdates >>= interpret . next
interpret (QualifyUpdate u next) = qualifyUpdate u >>= interpret . next
interpret (ReactToCommand c next) = reactToCommand c >>= interpret . next
interpret (ReactToCallback c next) = reactToCallback c >>= interpret . next
interpret (GetAuthorsSettings m next) = getAuthorsSettings m >>= interpret . next
interpret (EchoMessageNTimes m n next) = echoMessageNTimes m n >>= interpret . next
interpret (Done ret) = pure ret

fetchUpdatesDSL :: BotDSL api [Update api]
fetchUpdatesDSL = FetchUpdates Done

qualifyUpdateDSL :: Update api -> BotDSL api (Entity api)
qualifyUpdateDSL u = QualifyUpdate u Done

botLoop :: BotDSL a ret
botLoop = do
  updates <- fetchUpdatesDSL
  forM_ updates $ \u -> do
    e <- qualifyUpdateDSL u
    case e of
      ECommand cmd -> reactToCommandDSL cmd
      EMessage msg -> reactToMessageDSL msg
      ECallback cq -> reactToCallbackDSL cq
  botLoop

reactToMessageDSL :: Message api -> BotDSL api [Response api]
reactToMessageDSL msg = do
  settings <- getAuthorsSettingsDSL msg
  let n = DB.getEchoMultiplier settings
  echoMessageNTimesDSL msg n

getAuthorsSettingsDSL :: Message api -> BotDSL api DB.UserData
getAuthorsSettingsDSL msg = GetAuthorsSettings msg Done

echoMessageNTimesDSL :: Message api -> Int -> BotDSL api [Response api]
echoMessageNTimesDSL msg n = EchoMessageNTimes msg n Done

reactToCommandDSL :: Command api -> BotDSL api [Response api]
reactToCommandDSL cmd = ReactToCommand cmd Done

reactToCallbackDSL :: CallbackQuery api -> BotDSL api [Response api]
reactToCallbackDSL cq = ReactToCallback cq Done
