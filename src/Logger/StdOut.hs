{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger.StdOut (module Logger.Internal, withHandle, new, close) where

import qualified App.Error
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Data.Text.Extended as T (Text, tshow)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Effects.Log as Log (Priority, Verbosity, composeMessage)
import Logger.Internal
import qualified System.IO as IO (hFlush, stdout)

withHandle :: Log.Verbosity -> (Handle -> IO ()) -> IO ()
withHandle v = bracket (new v) close

new :: Log.Verbosity -> IO Handle
new verbosity = new_ verbosity `catch` rethrow
  where
    rethrow :: MonadThrow m => IOError -> m a
    rethrow = throwM . App.Error.loggerError . ("Logger Initiation Error: " <>) . T.tshow

new_ :: Log.Verbosity -> IO Handle
new_ v = do
  let hStdout = IO.stdout
  let doLog :: Log.Priority -> Text -> IO ()
      doLog p t = Log.composeMessage p t >>= T.hPutStrLn hStdout
  let getLog :: Log.Priority -> Text -> IO ()
      getLog p t = when (p >= v) (doLog p t) `catch` rethrow
  pure $ Handle {getLog}
  where
    rethrow :: MonadThrow m => IOError -> m a
    rethrow = throwM . App.Error.loggerError . ("Logger failed to write message to StdOut: " <>) . T.tshow

close :: Handle -> IO ()
close _hLog = IO.hFlush IO.stdout
