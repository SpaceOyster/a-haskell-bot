{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App.Env where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), asks)
import Data.Has (Has(..))
import Data.Kind (Type)
import qualified Data.Text as T
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

envLogDebug, envLogInfo, envLogWarning, envLogError ::
       (MonadIO m, MonadReader env m, Has Logger.Handle env) => T.Text -> m ()
envLogDebug t = grab @Logger.Handle >>= (`Logger.logDebug` t)

envLogInfo t = grab @Logger.Handle >>= (`Logger.logInfo` t)

envLogWarning t = grab @Logger.Handle >>= (`Logger.logWarning` t)

envLogError t = grab @Logger.Handle >>= (`Logger.logError` t)
