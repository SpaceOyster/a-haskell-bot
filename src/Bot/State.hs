{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bot.State
    ( BotState(..)
    , Config(..)
    , Handle(..)
    , new
    , getUserMultiplierM
    , getUserMultiplier
    , setUserMultiplier
    ) where

import App.Monad (envLogDebug, envLogInfo)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Function ((&))
import Data.Has (Has(..))
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map (Map, alter, findWithDefault)
import qualified Data.Text.Extended as T
import qualified Logger

newtype BotState =
    BotState
        { userSettings :: Map.Map H.Hash Int
        }

newtype Config =
    Config
        { echoMultiplier :: Int
        }

data Handle =
    Handle
        { state :: IORef BotState
        , echoMultiplier :: Int
        }

new :: (MonadIO m) => Config -> m Handle
new Config {echoMultiplier} = do
    state <- liftIO $ newIORef BotState {userSettings = mempty}
    pure Handle {state, echoMultiplier}

hGetState ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has Logger.Handle env)
    => Handle
    -> m BotState
hGetState hState = do
    envLogDebug "Getting BotState"
    liftIO . readIORef $ state hState

hSetState ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has Logger.Handle env)
    => Handle
    -> (BotState -> BotState)
    -> m ()
hSetState hState f = do
    envLogDebug "Setting BotState"
    envLogDebug "Appying state BotState mutating function"
    liftIO $ state hState `modifyIORef` f

getUserMultiplier ::
       ( H.Hashable u
       , MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has Logger.Handle env
       )
    => Handle
    -> u
    -> m Int
getUserMultiplier hState user = do
    st <- hGetState hState
    let drepeats = echoMultiplier (hState :: Handle)
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
    => Handle
    -> Maybe u
    -> m Int
getUserMultiplierM hState (Just u) = hState & getUserMultiplier $ u
getUserMultiplierM hState Nothing = do
    envLogInfo "No User info, returning default echo multiplier"
    pure $ echoMultiplier (hState :: Handle)

setUserMultiplier ::
       ( Show u
       , H.Hashable u
       , MonadIO m
       , MonadThrow m
       , MonadReader env m
       , Has Logger.Handle env
       )
    => Handle
    -> u
    -> Int
    -> m ()
setUserMultiplier hState user repeats = do
    envLogDebug $
        "Setting echo multiplier to: " <>
        T.tshow repeats <> "For User: " <> T.tshow user
    hState `hSetState` \st ->
        let uhash = H.hash user
            usettings = Map.alter (const $ Just repeats) uhash $ userSettings st
         in st {userSettings = usettings}
