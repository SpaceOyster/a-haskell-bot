{-# LANGUAGE KindSignatures #-}

module App.Monad where

import Data.Kind (Type)
import qualified Logger

newtype Env (m :: Type -> Type) =
    Env
        { envLogger :: Logger.Handle
        }
