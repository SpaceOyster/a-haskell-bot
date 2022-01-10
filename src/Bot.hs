{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot where

import App.Monad
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Function ((&))
import Data.Has (Has(..))
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.List.Extended (replaceSubseq)
import qualified Data.Map as Map (Map, alter, findWithDefault)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import qualified HTTP
import qualified Logger
import Prelude hiding (repeat)

data Handle apiHandle =
    Handle
        { hAPI :: apiHandle
        , state :: IORef BotState
        , strings :: Strings
        , echoMultiplier :: Int
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

hGetState ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has Logger.Handle env)
    => Handle s
    -> m BotState
hGetState hBot = do
    envLogDebug "Getting BotState"
    liftIO . readIORef $ state hBot

hSetState ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has Logger.Handle env)
    => Handle s
    -> (BotState -> BotState)
    -> m ()
hSetState hBot f = do
    envLogDebug "Setting BotState"
    envLogDebug "Appying state BotState mutating function"
    liftIO $ state hBot `modifyIORef` f

newtype BotState =
    BotState
        { userSettings :: Map.Map H.Hash Int
        }

getUserMultiplier ::
       ( H.Hashable u
       , MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has Logger.Handle env
       )
    => Handle s
    -> u
    -> m Int
getUserMultiplier hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = Bot.echoMultiplier hBot
        uhash = H.hash user
        repeats = Map.findWithDefault drepeats uhash $ userSettings st
    pure repeats

getUserMultiplierM ::
       ( H.Hashable u
       , MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has Logger.Handle env
       )
    => Handle s
    -> Maybe u
    -> m Int
getUserMultiplierM hBot (Just u) = hBot & getUserMultiplier $ u
getUserMultiplierM hBot Nothing = do
    envLogInfo "No User info, returning default echo multiplier"
    pure $ hBot & Bot.echoMultiplier

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
    mult <- hBot & getUserMultiplierM $ userM
    let prompt' = hBot & Bot.strings & Bot.repeat
    pure $ replaceSubseq prompt' "%n" (show mult)

setUserMultiplier ::
       ( Show u
       , H.Hashable u
       , MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has Logger.Handle env
       )
    => Handle s
    -> u
    -> Int
    -> m ()
setUserMultiplier hBot user repeats = do
    envLogDebug $
        "Setting echo multiplier to: " <>
        T.tshow repeats <> "For User: " <> T.tshow user
    hBot `Bot.hSetState` \st ->
        let uhash = H.hash user
            usettings = Map.alter (const $ Just repeats) uhash $ userSettings st
         in st {userSettings = usettings}

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
