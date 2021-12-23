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

type AppEnv = Env App

newtype App a =
    App
        { unApp :: ReaderT AppEnv IO a
        }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)
