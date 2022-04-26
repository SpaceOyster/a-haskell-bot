{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Bot where

import Control.Monad (ap, forM_, forever, liftM, (>=>))
import Control.Monad.Catch (MonadThrow (..))
import Data.Function ((&))
import qualified Data.Hashable as H
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.UsersDB as DB
import Text.Parsec as Parsec (many1, parse)
import Text.Parsec.Char as Parsec (digit, string)
import Text.Parsec.Text as Parsec.Text (Parser)

repeatPrompt ::
  (H.Hashable u, MonadThrow m, DB.MonadUsersDB m, BR.MonadBotReplies m) =>
  Maybe u ->
  m T.Text
repeatPrompt userM = do
  userData <- userM & DB.getUserDataM & DB.orDefaultData
  prompt <- BR.getReply BR.repeat
  pure $ BR.insertUserData userData prompt

data BotDSL api ret
  = FetchUpdates ([Entity api] -> BotDSL api ret)
  | ExecCommand BotCommand (Command api) (BotDSL api ret)
  | ReactToCallback (CallbackQuery api) (BotDSL api ret)
  | GetAuthorsSettings (Message api) (DB.UserData -> BotDSL api ret)
  | EchoMessageNTimes (Message api) Int (BotDSL api ret)
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
      ExecCommand bc c next -> ExecCommand bc c $ next >>= mk
      ReactToCallback c next -> ReactToCallback c $ next >>= mk
      GetAuthorsSettings m next -> GetAuthorsSettings m $ next >=> mk
      EchoMessageNTimes m n next -> EchoMessageNTimes m n $ next >>= mk

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
isCommand s = T.head s == '/'

loop :: EchoBotMonad m => m a
loop = interpret botLoop

data Entity api
  = EMessage (Message api)
  | ECommand BotCommand (Command api)
  | ECallback (CallbackQuery api)

newtype QueryData
  = QDRepeat Int
  deriving (Show)

parseQuery :: T.Text -> Maybe QueryData
parseQuery t = either (const Nothing) Just $ Parsec.parse queryParser "QueryData" t

queryParser :: Parsec.Text.Parser QueryData
queryParser =
  QDRepeat . read <$> (Parsec.string "repeat_" *> Parsec.many1 Parsec.digit)

encodeQuery :: QueryData -> T.Text
encodeQuery (QDRepeat n) = "repeat_" <> T.tshow n

class (Monad m) => EchoBotMonad m where
  type Message m
  type Command m
  type CallbackQuery m

  fetchUpdates :: m [Entity m]
  reactToCallback :: CallbackQuery m -> m ()
  getAuthorsSettings :: Message m -> m DB.UserData
  echoMessageNTimes :: Message m -> Int -> m ()
  execCommand :: BotCommand -> (Command m -> m ())

reactToMessage :: EchoBotMonad m => Message m -> m ()
reactToMessage msg = do
  settings <- getAuthorsSettings msg
  let n = DB.getEchoMultiplier settings
  Bot.echoMessageNTimes msg n

reactToCommand :: EchoBotMonad m => BotCommand -> Command m -> m ()
reactToCommand cmd c = do
  Bot.execCommand cmd c

reactToUpdate :: EchoBotMonad m => Entity m -> m ()
reactToUpdate update = do
  case update of
    Bot.ECommand cmd msg -> Bot.reactToCommand cmd msg
    Bot.EMessage msg -> Bot.reactToMessage msg
    Bot.ECallback cq -> Bot.reactToCallback cq

reactToUpdates :: EchoBotMonad m => [Entity m] -> m ()
reactToUpdates = mapM_ reactToUpdate

doBotThing :: EchoBotMonad m => m ()
doBotThing = forever (fetchUpdates >>= reactToUpdates)

interpret :: EchoBotMonad m => BotDSL m a -> m a
interpret (FetchUpdates next) = fetchUpdates >>= interpret . next
interpret (ExecCommand bc c next) = execCommand bc c >> interpret next
interpret (ReactToCallback c next) = reactToCallback c >> interpret next
interpret (GetAuthorsSettings m next) = getAuthorsSettings m >>= interpret . next
interpret (EchoMessageNTimes m n next) = echoMessageNTimes m n >> interpret next
interpret (Done ret) = pure ret

fetchUpdatesDSL :: BotDSL api [Entity api]
fetchUpdatesDSL = FetchUpdates Done

botLoop :: BotDSL a ret
botLoop = do
  updates <- fetchUpdatesDSL
  forM_ updates $ \case
    ECommand cmd ent -> reactToCommandDSL cmd ent
    EMessage msg -> reactToMessageDSL msg
    ECallback cq -> reactToCallbackDSL cq
  botLoop

reactToCommandDSL :: BotCommand -> Command api -> BotDSL api ()
reactToCommandDSL cmd c = do
  ExecCommand cmd c $ Done ()

reactToMessageDSL :: Message api -> BotDSL api ()
reactToMessageDSL msg = do
  settings <- getAuthorsSettingsDSL msg
  let n = DB.getEchoMultiplier settings
  echoMessageNTimesDSL msg n

getAuthorsSettingsDSL :: Message api -> BotDSL api DB.UserData
getAuthorsSettingsDSL msg = GetAuthorsSettings msg Done

echoMessageNTimesDSL :: Message api -> Int -> BotDSL api ()
echoMessageNTimesDSL msg n = EchoMessageNTimes msg n $ Done ()

reactToCallbackDSL :: CallbackQuery api -> BotDSL api ()
reactToCallbackDSL cq = ReactToCallback cq $ Done ()
