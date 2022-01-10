{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Monad where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader
import Data.Has (Has(..))
import Data.Kind (Type)
import qualified Data.Text as T
import qualified HTTP
import qualified Logger

data Env (m :: Type -> Type) =
    Env
        { envLogger :: Logger.Handle
        , envHTTP :: HTTP.Handle
        }

instance Has Logger.Handle (Env m) where
    obtain = envLogger

instance Has HTTP.Handle (Env m) where
    obtain = envHTTP

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
