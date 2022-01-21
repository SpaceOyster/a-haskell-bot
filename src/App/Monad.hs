{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad where

import App.Env (Env(..))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))

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
