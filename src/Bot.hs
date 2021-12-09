{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Bot where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Char (toLower)
import Data.Function ((&))
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.List.Extended (replaceSubseq)
import qualified Data.Map as Map (Map, alter, findWithDefault)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import qualified Logger as L
import Prelude hiding (repeat)

data Handle apiHandle =
    Handle
        { hAPI :: apiHandle
        , hLog :: L.Handle
        , state :: IORef BotState
        , strings :: Strings
        , echoMultiplier :: Int
        }

instance {-# OVERLAPPABLE #-} L.HasLog (Bot.Handle s) where
    getLog Bot.Handle {hLog} = L.getLog hLog

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

hGetState :: Handle s -> IO BotState
hGetState hBot = do
    L.logDebug hBot "Getting BotState"
    readIORef $ state hBot

hSetState :: Handle s -> (BotState -> BotState) -> IO ()
hSetState hBot f = do
    L.logDebug hBot "Setting BotState"
    L.logDebug hBot "Appying state BotState mutating function"
    state hBot `modifyIORef` f

newtype BotState =
    BotState
        { userSettings :: Map.Map H.Hash Int
        }

getUserMultiplier :: (H.Hashable u) => Handle s -> u -> IO Int
getUserMultiplier hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = Bot.echoMultiplier hBot
        uhash = H.hash user
        repeats = Map.findWithDefault drepeats uhash $ userSettings st
    pure repeats

getUserMultiplierM :: (H.Hashable u) => Handle s -> Maybe u -> IO Int
getUserMultiplierM hBot (Just u) = hBot & getUserMultiplier $ u
getUserMultiplierM hBot Nothing = do
    L.logInfo hBot "No User info, returning default echo multiplier"
    pure $ hBot & Bot.echoMultiplier

repeatPrompt :: (H.Hashable u) => Handle s -> Maybe u -> IO String
repeatPrompt hBot userM = do
    mult <- hBot & getUserMultiplierM $ userM
    let prompt' = hBot & Bot.strings & Bot.repeat
    pure $ replaceSubseq prompt' "%n" (show mult)

setUserMultiplier :: (Show u, H.Hashable u) => Handle s -> u -> Int -> IO ()
setUserMultiplier hBot user repeats = do
    L.logDebug hBot $
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

class (L.HasLog h) =>
      BotHandle h
    where
    type Update h
    fetchUpdates :: h -> IO [Update h]
    doBotThing :: h -> IO [Response h]
    doBotThing hBot = fetchUpdates hBot >>= reactToUpdates hBot
    data Entity h
    type Response h
    qualifyUpdate :: Update h -> Entity h
    reactToUpdate :: h -> Update h -> IO [Response h]
    reactToUpdates :: h -> [Update h] -> IO [Response h]
    reactToUpdates hBot updates = do
        L.logInfo hBot "processing each update"
        join <$> mapM (reactToUpdate hBot) updates
    type Message h
    execCommand :: h -> Command -> (Message h -> IO (Response h))
