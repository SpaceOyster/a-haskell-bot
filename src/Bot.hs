{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, evalStateT, lift)
import Data.Function ((&))
import qualified Data.Hashable as H
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP (MonadHTTP(..))
import qualified Effects.Log as Log (MonadLog, logInfo)
import qualified Effects.UsersDB as DB
import Prelude hiding (repeat)

data Handle apiHandle =
  Handle
    { hAPI :: apiHandle
    , replies :: Replies
    }

data StringsM =
  StringsM
    { helpM :: Maybe T.Text
    , greetingM :: Maybe T.Text
    , repeatM :: Maybe T.Text
    , unknownM :: Maybe T.Text
    , settingsSavedM :: Maybe T.Text
    }
  deriving (Show)

data Replies =
  Replies
    { help :: T.Text
    , greeting :: T.Text
    , repeat :: T.Text
    , unknown :: T.Text
    , settingsSaved :: T.Text
    }
  deriving (Show)

fromStrinsM :: StringsM -> Replies
fromStrinsM StringsM {..} =
  Replies
    { help = fromMaybe "" helpM
    , greeting = fromMaybe "" greetingM
    , repeat = fromMaybe "" repeatM
    , unknown = fromMaybe "" unknownM
    , settingsSaved = fromMaybe "" settingsSavedM
    }

instance Semigroup StringsM where
  s0 <> s1 =
    StringsM
      { helpM = helpM s0 <|> helpM s1
      , greetingM = greetingM s0 <|> greetingM s1
      , repeatM = repeatM s0 <|> repeatM s1
      , unknownM = unknownM s0 <|> unknownM s1
      , settingsSavedM = settingsSavedM s0 <|> settingsSavedM s1
      }

instance Monoid StringsM where
  mempty =
    StringsM
      { helpM = mempty
      , greetingM = mempty
      , repeatM = mempty
      , unknownM = mempty
      , settingsSavedM = mempty
      }

repeatPrompt ::
     (H.Hashable u, MonadIO m, MonadThrow m, DB.MonadUsersDB m)
  => Handle s
  -> Maybe u
  -> m T.Text
repeatPrompt hBot userM = do
  mult <- DB.getUserMultiplierM userM
  let prompt' = hBot & Bot.replies & Bot.repeat
  pure $ T.replace "%n" (T.tshow mult) prompt'

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
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

class (Monoid (APIState h)) =>
      BotHandle h
  where
  type Update h
  type APIState h
  loop ::
       ( MonadIO m
       , MonadThrow m
       , HTTP.MonadHTTP m
       , Log.MonadLog m
       , DB.MonadUsersDB m
       )
    => StateT (APIState h) m h
    -> Int
    -> m ()
  loop bMonad period =
    flip evalStateT mempty $ do
      hBot <- bMonad
      forever $ Bot.doBotThing hBot >> liftIO (threadDelay period)
      pure ()
  fetchUpdates ::
       (MonadIO m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m)
    => h
    -> StateT (APIState h) m [Update h]
  doBotThing ::
       ( MonadIO m
       , MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       )
    => h
    -> StateT (APIState h) m [Response h]
  doBotThing hBot = fetchUpdates hBot >>= reactToUpdates hBot
  data Entity h
  type Response h
  qualifyUpdate :: Update h -> Entity h
  reactToUpdate ::
       ( MonadIO m
       , MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       )
    => h
    -> Update h
    -> StateT (APIState h) m [Response h]
  reactToUpdates ::
       ( MonadIO m
       , MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       )
    => h
    -> [Update h]
    -> StateT (APIState h) m [Response h]
  reactToUpdates hBot updates = do
    lift $ Log.logInfo "processing each update"
    join <$> mapM (reactToUpdate hBot) updates
  type Message h
  execCommand ::
       ( MonadIO m
       , MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       )
    => h
    -> Command
    -> (Message h -> StateT (APIState h) m (Response h))
