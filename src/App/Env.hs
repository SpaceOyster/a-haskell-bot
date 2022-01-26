{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import Control.Monad.Reader (MonadReader(..), asks)
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

grab ::
     forall field env m. (MonadReader env m, Has field env)
  => m field
grab = asks $ obtain @field
