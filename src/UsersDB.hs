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
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map (Map, alter, insert, lookup)
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

getUserData :: (H.Hashable u, MonadIO m) => Handle -> u -> m (Maybe DB.UserData)
getUserData hDB user = do
  uMap <- hGetUsersMap hDB
  let uhash = H.hash user
  pure $ Map.lookup uhash $ getUsersMap uMap

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

setUserData :: (H.Hashable u, MonadIO m) => Handle -> u -> DB.UserData -> m ()
setUserData hDB user udata =
  hModifyUsersMap hDB (UsersMap . Map.insert (H.hash user) udata . getUsersMap)
