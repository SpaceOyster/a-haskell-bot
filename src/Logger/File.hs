{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger.File (module Logger.Internal, withHandle, new) where

import qualified App.Error
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (when, (<=<))
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Data.Text.Extended as T (Text, tshow)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Effects.Log as Log (Priority, Verbosity, composeMessage)
import Logger.Internal
import qualified System.IO as IO
  ( FilePath,
    Handle,
    IOMode (..),
    withFile,
  )

withHandle :: Log.Verbosity -> IO.FilePath -> (Handle -> IO ()) -> IO ()
withHandle v path io = IO.withFile path IO.AppendMode (io <=< new v)

new :: Log.Verbosity -> IO.Handle -> IO Handle
new verbosity hFile = new_ verbosity hFile `catch` rethrow
  where
    rethrow :: MonadThrow m => IOError -> m a
    rethrow = throwM . App.Error.loggerError . ("Logger Initiation Error: " <>) . T.tshow

new_ :: Log.Verbosity -> IO.Handle -> IO Handle
new_ verbosity hFile = do
  mutex <- newMVar ()
  let getLog :: Log.Priority -> Text -> IO ()
      getLog p t = getLog_ verbosity mutex hFile p t `catch` rethrow
  pure $ Handle {getLog}
  where
    rethrow :: MonadThrow m => IOError -> m a
    rethrow = throwM . App.Error.loggerError . ("Logger failed to append to file: " <>) . T.tshow

doLog_ :: MVar () -> IO.Handle -> Log.Priority -> Text -> IO ()
doLog_ mutex hFile priority t =
  withMVar mutex $ \_ -> Log.composeMessage priority t >>= T.hPutStrLn hFile

getLog_ :: Log.Verbosity -> MVar () -> IO.Handle -> Log.Priority -> Text -> IO ()
getLog_ verbosity mutex hFile p t =
  when (p >= verbosity) (doLog_ mutex hFile p t)
