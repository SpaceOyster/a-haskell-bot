{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Bot where

import App.Env (grab)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, forever)
import Data.Function ((&))
import Data.Has (Has(..))
import qualified Data.Hashable as H
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import qualified Effects.Log as Log (MonadLog, logInfo)
import qualified HTTP
import Prelude hiding (repeat)
import qualified UsersDB (Handle(..), getUserMultiplierM)

data Handle apiHandle =
  Handle
    { hAPI :: apiHandle
    , strings :: Strings
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

data Strings =
  Strings
    { help :: T.Text
    , greeting :: T.Text
    , repeat :: T.Text
    , unknown :: T.Text
    , settingsSaved :: T.Text
    }
  deriving (Show)

fromStrinsM :: StringsM -> Strings
fromStrinsM StringsM {..} =
  Strings
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
     ( H.Hashable u
     , MonadIO m
     , MonadThrow m
     , MonadReader env m
     , Has UsersDB.Handle env
     )
  => Handle s
  -> Maybe u
  -> m T.Text
repeatPrompt hBot userM = do
  hDB <- grab @UsersDB.Handle
  mult <- UsersDB.getUserMultiplierM hDB userM
  let prompt' = hBot & Bot.strings & Bot.repeat
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

loop ::
     ( MonadIO m
     , MonadThrow m
     , Bot.BotHandle a
     , MonadReader env m
     , Has HTTP.Handle env
     , Has UsersDB.Handle env
     , Log.MonadLog m
     )
  => a
  -> Int
  -> m ()
loop hBot period = forever $ Bot.doBotThing hBot >> liftIO (threadDelay period)

class BotHandle h where
  type Update h
  fetchUpdates ::
       ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has HTTP.Handle env
       , Log.MonadLog m
       )
    => h
    -> m [Update h]
  doBotThing ::
       ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has HTTP.Handle env
       , Has UsersDB.Handle env
       , Log.MonadLog m
       )
    => h
    -> m [Response h]
  doBotThing hBot = fetchUpdates hBot >>= reactToUpdates hBot
  data Entity h
  type Response h
  qualifyUpdate :: Update h -> Entity h
  reactToUpdate ::
       ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has HTTP.Handle env
       , Has UsersDB.Handle env
       , Log.MonadLog m
       )
    => h
    -> Update h
    -> m [Response h]
  reactToUpdates ::
       ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has HTTP.Handle env
       , Has UsersDB.Handle env
       , Log.MonadLog m
       )
    => h
    -> [Update h]
    -> m [Response h]
  reactToUpdates hBot updates = do
    Log.logInfo "processing each update"
    join <$> mapM (reactToUpdate hBot) updates
  type Message h
  execCommand ::
       ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has HTTP.Handle env
       , Has UsersDB.Handle env
       , Log.MonadLog m
       )
    => h
    -> Command
    -> (Message h -> m (Response h))
