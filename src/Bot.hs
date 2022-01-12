{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot where

import App.Monad (envLogInfo)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, forever)
import Data.Char (toLower)
import Data.Function ((&))
import Data.Has (Has(..))
import qualified Data.Hashable as H
import Data.List.Extended (replaceSubseq)
import Data.Maybe (fromMaybe)
import qualified HTTP
import qualified Logger
import Prelude hiding (repeat)
import qualified UsersDB (Handle(..), getUserMultiplierM)

data Handle apiHandle =
    Handle
        { hAPI :: apiHandle
        , state :: UsersDB.Handle
        , strings :: Strings
        }

data StringsM =
    StringsM
        { helpM :: Maybe String
        , greetingM :: Maybe String
        , repeatM :: Maybe String
        , unknownM :: Maybe String
        , settingsSavedM :: Maybe String
        }
    deriving (Show)

data Strings =
    Strings
        { help :: String
        , greeting :: String
        , repeat :: String
        , unknown :: String
        , settingsSaved :: String
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
       , Has Logger.Handle env
       )
    => Handle s
    -> Maybe u
    -> m String
repeatPrompt hBot userM = do
    mult <- UsersDB.getUserMultiplierM (state hBot) userM
    let prompt' = hBot & Bot.strings & Bot.repeat
    pure $ replaceSubseq prompt' "%n" (show mult)

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
data Command
    = Start
    | Help
    | Repeat
    | UnknownCommand
    deriving (Show, Enum, Bounded)

describe :: Command -> String
describe Start = "Greet User"
describe Help = "Show help text"
describe Repeat = "Set echo multiplier"
describe UnknownCommand = "Unknown Command"

parseCommand :: String -> Command
parseCommand s =
    case toLower <$> s of
        "start" -> Start
        "help" -> Help
        "repeat" -> Repeat
        _ -> UnknownCommand

isCommand :: String -> Bool
isCommand "" = False
isCommand s = (== '/') . head $ s

loop ::
       ( MonadIO m
       , MonadThrow m
       , Bot.BotHandle a
       , MonadReader env m
       , Has Logger.Handle env
       , Has HTTP.Handle env
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
           , Has Logger.Handle env
           , Has HTTP.Handle env
           )
        => h
        -> m [Update h]
    doBotThing ::
           ( MonadIO m
           , MonadThrow m
           , MonadReader env m
           , Has Logger.Handle env
           , Has HTTP.Handle env
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
           , Has Logger.Handle env
           , Has HTTP.Handle env
           )
        => h
        -> Update h
        -> m [Response h]
    reactToUpdates ::
           ( MonadIO m
           , MonadThrow m
           , MonadReader env m
           , Has Logger.Handle env
           , Has HTTP.Handle env
           )
        => h
        -> [Update h]
        -> m [Response h]
    reactToUpdates hBot updates = do
        envLogInfo "processing each update"
        join <$> mapM (reactToUpdate hBot) updates
    type Message h
    execCommand ::
           ( MonadIO m
           , MonadThrow m
           , MonadReader env m
           , Has Logger.Handle env
           , Has HTTP.Handle env
           )
        => h
        -> Command
        -> (Message h -> m (Response h))
