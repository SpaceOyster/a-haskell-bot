{-# LANGUAGE NamedFieldPuns #-}

module Test.Handlers.UsersDB where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (newIORef)
import Handlers.UsersDB as DB
  ( Handle (..),
    UserData (UserData),
    UsersMap (..),
  )

data Config = Config
  { defaultEchoMultiplier :: Maybe Int,
    defaultDB :: Maybe DB.UsersMap
  }

new :: (MonadIO m) => m DB.Handle
new = do
  let defaultUserData = UserData 1
  state <- liftIO $ newIORef DB.UsersMap {DB.getUsersMap = mempty}
  pure $ DB.Handle {state, defaultUserData}
