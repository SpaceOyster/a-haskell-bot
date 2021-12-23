{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Monad where

import Control.Monad.Reader
import Data.Kind (Type)
import qualified Logger

newtype Env (m :: Type -> Type) =
    Env
        { envLogger :: Logger.Handle
        }

class Has field env where
    obtain :: env -> field

instance Has Logger.Handle (Env m) where
    obtain = envLogger

type AppEnv = Env App

newtype App a =
    App
        { unApp :: ReaderT AppEnv IO a
        }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

runApp :: AppEnv -> App a -> IO a
runApp env = (`runReaderT` env) . unApp
