{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Logger (module Logger.Internal, Config (..), withHandle, withHandlePure) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.Aeson as Aeson (FromJSON (..), withObject, (.!=), (.:?))
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import qualified Data.Text.Extended as T (Text)
import qualified Effects.Log as Log (Priority (..), Verbosity)
import qualified Logger.File
import Logger.Internal
import qualified Logger.StdOut

data Config = Config
  { file :: Maybe FilePath,
    verbosity :: Log.Verbosity
  }
  deriving (Show)

instance Semigroup Config where
  c0 <> c1 =
    Config
      { file = file c0 <|> file c1,
        verbosity = verbosity c0 `max` verbosity c1
      }

instance Monoid Config where
  mempty = Config {file = mempty, verbosity = Log.Info}

instance Aeson.FromJSON Config where
  parseJSON =
    Aeson.withObject "FromJSON Logger.Config" $ \o -> do
      file <- o Aeson..:? "file"
      verbosity <- o Aeson..:? "verbosity" Aeson..!= Log.Info
      pure $ Config {..}

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle Config {..} = case file of
  Just f -> Logger.File.withHandle verbosity f
  Nothing -> Logger.StdOut.withHandle verbosity

withHandlePure :: Config -> (Handle -> IO a) -> IO (a, [(Log.Priority, T.Text)])
withHandlePure Config {..} io = do
  logRef <- newIORef []
  let logAction p t =
        when (p >= verbosity) $
          atomicModifyIORef logRef $ \l -> ((p, t) : l, ())
  x <- io $ Handle {getLog = logAction}
  logMsgs <- readIORef logRef
  pure (x, reverse logMsgs)
