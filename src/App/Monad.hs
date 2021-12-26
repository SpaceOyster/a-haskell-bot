{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Monad where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader
import Data.Kind (Type)
import qualified Data.Text as T
import qualified Logger

newtype Env (m :: Type -> Type) =
    Env
        { envLogger :: Logger.Handle
        }

class Has field env where
    obtain :: env -> field

instance Has Logger.Handle (Env m) where
    obtain = envLogger

grab ::
       forall field env m. (MonadReader env m, Has field env)
    => m field
grab = asks $ obtain @field

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

envLogDebug, envLogInfo, envLogWarning, envLogError ::
       (MonadIO m, MonadReader env m, Has Logger.Handle env) => T.Text -> m ()
envLogDebug t = grab @Logger.Handle >>= (`Logger.logDebug` t)

envLogInfo t = grab @Logger.Handle >>= (`Logger.logInfo` t)

envLogWarning t = grab @Logger.Handle >>= (`Logger.logWarning` t)

envLogError t = grab @Logger.Handle >>= (`Logger.logError` t)
