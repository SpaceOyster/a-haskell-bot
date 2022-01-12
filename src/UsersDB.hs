{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module UsersDB
    ( UsersMap(..)
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

newtype UsersMap =
    UsersMap
        { getUsersMap :: Map.Map H.Hash Int
        }

newtype Config =
    Config
        { echoMultiplier :: Int
        }

data Handle =
    Handle
        { state :: IORef UsersMap
        , echoMultiplier :: Int
        }

new :: (MonadIO m) => Config -> m Handle
new Config {echoMultiplier} = do
    state <- liftIO $ newIORef UsersMap {getUsersMap = mempty}
    pure Handle {state, echoMultiplier}

hGetState :: (MonadIO m) => Handle -> m UsersMap
hGetState hState = liftIO . readIORef $ state hState

hModifyState :: (MonadIO m) => Handle -> (UsersMap -> UsersMap) -> m ()
hModifyState hState f = liftIO $ state hState `modifyIORef` f

getUserMultiplier :: (H.Hashable u, MonadIO m) => Handle -> u -> m Int
getUserMultiplier hState user = do
    st <- hGetState hState
    let defaultMultiplier = echoMultiplier (hState :: Handle)
        uhash = H.hash user
        repeats = Map.findWithDefault defaultMultiplier uhash $ getUsersMap st
    pure repeats

getUserMultiplierM :: (H.Hashable u, MonadIO m) => Handle -> Maybe u -> m Int
getUserMultiplierM hState (Just u) = getUserMultiplier hState u
getUserMultiplierM hState Nothing = pure $ echoMultiplier (hState :: Handle)

setUserMultiplier ::
       (Show u, H.Hashable u, MonadIO m) => Handle -> u -> Int -> m ()
setUserMultiplier hState user repeats =
    hModifyState hState $ \st ->
        let uhash = H.hash user
            usettings = Map.alter (const $ Just repeats) uhash $ getUsersMap st
         in st {getUsersMap = usettings}
