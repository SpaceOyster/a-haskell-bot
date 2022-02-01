{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import qualified Bot.Replies as Bot
import Data.Has (Has(..))
import Data.Kind (Type)
import qualified HTTP
import qualified Logger
import qualified UsersDB

data Env (m :: Type -> Type) =
  Env
    { envLogger :: Logger.Handle
    , envHTTP :: HTTP.Handle
    , envUsersDB :: UsersDB.Handle
    }

instance Has Logger.Handle (Env m) where
  obtain = envLogger

instance Has HTTP.Handle (Env m) where
  obtain = envHTTP

instance Has UsersDB.Handle (Env m) where
  obtain = envUsersDB

