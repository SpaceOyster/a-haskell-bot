{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module UsersDB
  ( UsersMap (..),
    Config (..),
    Handle (..),
    DB.UserData (..),
    new,
    getUserData,
    modifyUserData,
    setUserData,
  )
where

import App.Error (AppError, dbError)
import Control.Monad.Catch
  ( MonadCatch,
    MonadThrow,
    catch,
    throwM,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Hashable as H
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as Map (Map, alter, insert, lookup)
import qualified Data.Text.Extended as T (tshow)
import qualified Effects.UsersDB as DB

newtype UsersMap = UsersMap
  { getUsersMap :: Map.Map H.Hash DB.UserData
  }

newtype Config = Config
  { defaultEchoMultiplier :: Int
  }

data Handle = Handle
  { state :: IORef UsersMap,
    defaultUserData :: DB.UserData
  }

hGetUsersMap :: (MonadIO m) => Handle -> m UsersMap
hGetUsersMap hDB = liftIO . readIORef $ state hDB

hModifyUsersMap :: (MonadIO m) => Handle -> (UsersMap -> UsersMap) -> m ()
hModifyUsersMap hDB f = liftIO $ state hDB `modifyIORef` f

rethrow :: (MonadThrow m) => IOError -> m a
rethrow = throwM . toDbError

toDbError :: IOError -> AppError
toDbError = dbError . T.tshow

catchToRethrow :: (MonadCatch m) => m a -> m a
catchToRethrow = flip catch rethrow

new :: (MonadIO m, MonadCatch m) => Config -> m Handle
new Config {defaultEchoMultiplier} = catchToRethrow $ do
  state <- liftIO $ newIORef UsersMap {getUsersMap = mempty}
  let defaultUserData =
        DB.UserData {DB.getEchoMultiplier = defaultEchoMultiplier}
  pure Handle {state, defaultUserData}

getUserData :: (H.Hashable u, MonadIO m, MonadCatch m) => Handle -> u -> m (Maybe DB.UserData)
getUserData hDB user = catchToRethrow $ do
  uMap <- hGetUsersMap hDB
  let uhash = H.hash user
  pure $ Map.lookup uhash $ getUsersMap uMap

modifyUserData ::
  (H.Hashable u, MonadIO m, MonadCatch m) =>
  Handle ->
  u ->
  (DB.UserData -> DB.UserData) ->
  m ()
modifyUserData hDB user morph = catchToRethrow $
  hModifyUsersMap hDB $ \uMap ->
    let uhash = H.hash user
        m = getUsersMap uMap
        usettings = Map.alter (fmap morph) uhash m
     in uMap {getUsersMap = usettings}

setUserData :: (H.Hashable u, MonadIO m, MonadCatch m) => Handle -> u -> DB.UserData -> m ()
setUserData hDB user udata =
  catchToRethrow $
    hModifyUsersMap hDB (UsersMap . Map.insert (H.hash user) udata . getUsersMap)
