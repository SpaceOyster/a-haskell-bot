{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module App.Monad where

import App.Env (Env(..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Data.Has (grab)
import qualified Effects.HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import qualified HTTP
import qualified Logger
import qualified UsersDB

type AppEnv = Env App

newtype App a =
  App
    { unApp :: ReaderT AppEnv IO a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadReader AppEnv
           )

runApp :: AppEnv -> App a -> IO a
runApp env = (`runReaderT` env) . unApp

instance Log.MonadLog App where
  doLog p t = do
    hLog <- grab @Logger.Handle
    let logAction = Logger.getLog hLog
    App . liftIO $ logAction p t

instance Effects.HTTP.MonadHTTP App where
  sendRequest req = do
    hHTTP <- grab @HTTP.Handle
    let sendAction = HTTP.sendRequest hHTTP
    App . liftIO $ sendAction req

instance DB.MonadUsersDB App where
  defaultUserData = do
    hUsersDB <- grab @UsersDB.Handle
    pure $ UsersDB.defaultUserData hUsersDB
  getUserData user = do
    hUsersDB <- grab @UsersDB.Handle
    let getAction = UsersDB.getUserData hUsersDB
    App . liftIO $ getAction user
  modifyUserData user morph = do
    hUsersDB <- grab @UsersDB.Handle
    let modifyAction = UsersDB.modifyUserData hUsersDB
    App . liftIO $ modifyAction user morph
