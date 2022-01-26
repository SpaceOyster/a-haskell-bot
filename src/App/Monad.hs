{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module App.Monad where

import App.Env (Env(..), grab)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import qualified Effects.Log as Log
import qualified Logger

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
