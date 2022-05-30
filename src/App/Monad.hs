{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module App.Monad where

import App.Env (Env (..))
import App.Error (AppError)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Has (grab)
import qualified Data.Text.Extended as T (tshow)
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import qualified Handlers.HTTP as HTTP
import qualified Handlers.Logger as Logger
import qualified Handlers.UsersDB as UsersDB

type AppEnv = Env App

newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader AppEnv
    )

runApp :: AppEnv -> App () -> IO ()
runApp env app =
  flip runReaderT env . unApp $
    app `catch` \e ->
      Log.logError . mconcat $
        [ "Unable to proceed: \n",
          T.tshow (e :: AppError),
          "\nClosing application."
        ]

evalApp :: AppEnv -> App a -> IO a
evalApp env app = runReaderT (unApp app) env

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
  setUserData user udata = do
    hUsersDB <- grab @UsersDB.Handle
    let setAction = UsersDB.setUserData hUsersDB
    App . liftIO $ setAction user udata

instance BR.MonadBotReplies App where
  getReplies = grab @BR.Replies
