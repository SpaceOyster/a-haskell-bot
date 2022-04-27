{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import Data.Has (Has (..))
import Data.Kind (Type)
import qualified Effects.BotReplies as BR
import qualified Handlers.HTTP as HTTP
import qualified Handlers.Logger as Logger
import qualified Handlers.UsersDB as UsersDB

data Env (m :: Type -> Type) = Env
  { envLogger :: Logger.Handle,
    envHTTP :: HTTP.Handle,
    envUsersDB :: UsersDB.Handle,
    envBotReplies :: BR.Replies
  }

instance Has Logger.Handle (Env m) where
  obtain = envLogger

instance Has HTTP.Handle (Env m) where
  obtain = envHTTP

instance Has UsersDB.Handle (Env m) where
  obtain = envUsersDB

instance Has BR.Replies (Env m) where
  obtain = envBotReplies
