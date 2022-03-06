{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bot where

import Bot.Replies as Bot
import Control.Monad (ap, forM_, forever, join, liftM, (>=>))
import Control.Monad.Catch (MonadThrow)
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

reactToUpdate :: forall m. EchoBotMonad m => Update m -> m [Response m]
reactToUpdate update = do
  qu <- qualifyUpdate @m update
  case qu of
    Bot.ECommand msg -> Bot.reactToCommand msg
    Bot.EMessage msg -> Bot.reactToMessage msg
    Bot.ECallback cq -> Bot.reactToCallback cq

reactToUpdates :: EchoBotMonad m => [Update m] -> m [Response m]
reactToUpdates updates = do
  join <$> mapM reactToUpdate updates

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

fetchUpdates' :: BotDSL api [Update api]
fetchUpdates' = FetchUpdates Done

qualifyUpdate' :: Update api -> BotDSL api (Entity api)
qualifyUpdate' u = QualifyUpdate u Done

botLoop :: BotDSL a ret
botLoop = do
  updates <- fetchUpdates'
  forM_ updates $ \u -> do
    e <- qualifyUpdate' u
    case e of
      ECommand cmd -> reactToCommand' cmd
      EMessage msg -> reactToMessage' msg
      ECallback cq -> reactToCallback' cq
  botLoop

reactToMessage' :: Message api -> BotDSL api [Response api]
reactToMessage' msg = do
  settings <- getAuthorsSettings' msg
  let n = DB.getEchoMultiplier settings
  echoMessageNTimes' msg n

getAuthorsSettings' :: Message api -> BotDSL api DB.UserData
getAuthorsSettings' msg = GetAuthorsSettings msg Done

echoMessageNTimes' :: Message api -> Int -> BotDSL api [Response api]
echoMessageNTimes' msg n = EchoMessageNTimes msg n Done

reactToCommand' :: Command api -> BotDSL api [Response api]
reactToCommand' cmd = ReactToCommand cmd Done

reactToCallback' :: CallbackQuery api -> BotDSL api [Response api]
reactToCallback' cq = ReactToCallback cq Done
