{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module UsersDB
    ( BotState(..)
    , Config(..)
    , Handle(..)
    , new
    , getUserMultiplierM
    , getUserMultiplier
    , setUserMultiplier
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map (Map, alter, findWithDefault)

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

hGetState :: (MonadIO m) => Handle -> m BotState
hGetState hState = liftIO . readIORef $ state hState

hModifyState :: (MonadIO m) => Handle -> (BotState -> BotState) -> m ()
hModifyState hState f = liftIO $ state hState `modifyIORef` f

getUserMultiplier :: (H.Hashable u, MonadIO m) => Handle -> u -> m Int
getUserMultiplier hState user = do
    st <- hGetState hState
    let drepeats = echoMultiplier (hState :: Handle)
        uhash = H.hash user
        repeats = Map.findWithDefault drepeats uhash $ userSettings st
    pure repeats

getUserMultiplierM :: (H.Hashable u, MonadIO m) => Handle -> Maybe u -> m Int
getUserMultiplierM hState (Just u) = getUserMultiplier hState u
getUserMultiplierM hState Nothing = pure $ echoMultiplier (hState :: Handle)

setUserMultiplier ::
       (Show u, H.Hashable u, MonadIO m) => Handle -> u -> Int -> m ()
setUserMultiplier hState user repeats =
    hModifyState hState $ \st ->
        let uhash = H.hash user
            usettings = Map.alter (const $ Just repeats) uhash $ userSettings st
         in st {userSettings = usettings}
