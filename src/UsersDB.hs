{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module UsersDB
  ( UsersMap(..)
  , Config(..)
  , Handle(..)
  , DB.UserData(..)
  , new
  , getUserData
  , modifyUserData
  , setUserData
  , getUserMultiplierM
  , getUserMultiplier
  , setUserMultiplier
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map (Map, alter, lookup)
import Data.Maybe (fromMaybe)
import qualified Effects.UsersDB as DB

newtype UsersMap =
  UsersMap
    { getUsersMap :: Map.Map H.Hash DB.UserData
    }

newtype Config =
  Config
    { defaultEchoMultiplier :: Int
    }

data Handle =
  Handle
    { state :: IORef UsersMap
    , defaultUserData :: DB.UserData
    }

new :: (MonadIO m) => Config -> m Handle
new Config {defaultEchoMultiplier} = do
  state <- liftIO $ newIORef UsersMap {getUsersMap = mempty}
  let defaultUserData =
        DB.UserData {DB.getEchoMultiplier = defaultEchoMultiplier}
  pure Handle {state, defaultUserData}

hGetUsersMap :: (MonadIO m) => Handle -> m UsersMap
hGetUsersMap hDB = liftIO . readIORef $ state hDB

hModifyUsersMap :: (MonadIO m) => Handle -> (UsersMap -> UsersMap) -> m ()
hModifyUsersMap hDB f = liftIO $ state hDB `modifyIORef` f

getUserData :: (H.Hashable u, MonadIO m) => Handle -> u -> m DB.UserData
getUserData hDB user = do
  uMap <- hGetUsersMap hDB
  let defaultSettings = defaultUserData hDB
      uhash = H.hash user
      maybeSettings = Map.lookup uhash $ getUsersMap uMap
  pure $ fromMaybe defaultSettings maybeSettings

getUserMultiplier :: (H.Hashable u, MonadIO m) => Handle -> u -> m Int
getUserMultiplier hDB user = DB.getEchoMultiplier <$> getUserData hDB user

getUserMultiplierM :: (H.Hashable u, MonadIO m) => Handle -> Maybe u -> m Int
getUserMultiplierM hDB (Just u) = getUserMultiplier hDB u
getUserMultiplierM hDB Nothing =
  pure . DB.getEchoMultiplier $ defaultUserData hDB

modifyUserData ::
     (H.Hashable u, MonadIO m)
  => Handle
  -> u
  -> (DB.UserData -> DB.UserData)
  -> m ()
modifyUserData hDB user morph =
  hModifyUsersMap hDB $ \uMap ->
    let uhash = H.hash user
        m = getUsersMap uMap
        usettings = Map.alter (fmap morph) uhash m
     in uMap {getUsersMap = usettings}

setUserData ::
     (Show u, H.Hashable u, MonadIO m) => Handle -> u -> DB.UserData -> m ()
setUserData hDB user us = modifyUserData hDB user $ const us

setUserMultiplier ::
     (Show u, H.Hashable u, MonadIO m) => Handle -> u -> Int -> m ()
setUserMultiplier hDB user multiplier =
  modifyUserData hDB user $ \us -> us {DB.getEchoMultiplier = multiplier}
