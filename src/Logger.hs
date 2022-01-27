{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logger
  ( Config(..)
  , Handle(..)
  , Log.Priority(..)
  , withHandle
  , withHandlePure
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A (FromJSON(..), (.!=), (.:?), withObject)
import Data.Has (Has(..))
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import qualified Data.Text.Extended as T (Text, pack, toUpper, tshow)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Data.Time.LocalTime as Time (getZonedTime)
import qualified Effects.Log as Log (Priority(..))
import Prelude hiding (log)
import qualified System.IO as IO
  ( FilePath
  , Handle
  , IOMode(..)
  , hFlush
  , stdout
  , withFile
  )

type Verbosity = Log.Priority

prioToText :: Log.Priority -> T.Text
prioToText = T.toUpper . T.tshow

data Config =
  Config
    { file :: Maybe FilePath
    , verbosity :: Verbosity
    }
  deriving (Show)

instance Semigroup Config where
  c0 <> c1 =
    Config
      {file = file c0 <|> file c1, verbosity = verbosity c0 `max` verbosity c1}

instance Monoid Config where
  mempty = Config {file = mempty, verbosity = Log.Info}

instance A.FromJSON Config where
  parseJSON =
    A.withObject "FromJSON Logger.Config" $ \o -> do
      file <- o A..:? "file"
      verbosity <- o A..:? "verbosity" A..!= Log.Info
      pure $ Config {..}

newtype Handle =
  Handle
    { getLog :: Log.Priority -> T.Text -> IO ()
    }

instance Has Handle Handle where
  obtain = id

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle Config {..} io =
  case file of
    Just f -> withFileLog verbosity f io
    Nothing -> withStdoutLog verbosity io

withFileLog :: Verbosity -> IO.FilePath -> (Handle -> IO ()) -> IO ()
withFileLog v path io = IO.withFile path IO.AppendMode (io <=< newFileLog v)

newFileLog :: Verbosity -> IO.Handle -> IO Handle
newFileLog v hFile = do
  mutex <- newMVar ()
  let doLog p t =
        withMVar mutex $ \() -> composeMessage p t >>= T.hPutStrLn hFile
  let getLog = \p t -> when (p >= v) $ doLog p t
  pure $ Handle {getLog}

newStdoutLog :: Verbosity -> IO Handle
newStdoutLog v = do
  let hStdout = IO.stdout
  let getLog =
        \p t -> when (p >= v) $ composeMessage p t >>= T.hPutStrLn hStdout
  pure $ Handle {getLog}

closeStdoutLog :: Handle -> IO ()
closeStdoutLog _hLog = IO.hFlush IO.stdout

withStdoutLog :: Verbosity -> (Handle -> IO ()) -> IO ()
withStdoutLog v = bracket (newStdoutLog v) closeStdoutLog

withHandlePure :: Config -> (Handle -> IO a) -> IO (a, [(Log.Priority, T.Text)])
withHandlePure Config {..} io = do
  logRef <- newIORef []
  let logAction p t =
        when (p >= verbosity) $
        atomicModifyIORef logRef $ \l -> ((p, t) : l, ())
  x <- io $ Handle {getLog = logAction}
  logMsgs <- readIORef logRef
  pure (x, reverse logMsgs)

composeMessage :: Log.Priority -> T.Text -> IO T.Text
composeMessage p t = do
  ts <- timeStamp
  pure $ ts <> " " <> prioToText p <> " " <> t

timeStamp :: IO T.Text
timeStamp = do
  time <- Time.getZonedTime
  pure . T.pack $ Time.formatTime Time.defaultTimeLocale "%b %d %X %Z" time

logDebug, logInfo, logWarning, logError :: MonadIO m => Handle -> T.Text -> m ()
logDebug h = liftIO . getLog h Log.Debug

logInfo h = liftIO . getLog h Log.Info

logWarning h = liftIO . getLog h Log.Warning

logError h = liftIO . getLog h Log.Error
