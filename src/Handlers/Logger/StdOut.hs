{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Logger.StdOut
  ( module Handlers.Logger.Internal,
    withHandle,
    new,
    close,
  )
where

import App.Error (AppError, loggerError)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Catch (catch, throwM)
import Data.Text.Extended as T (Text, tshow)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Effects.Log as Log (Priority, Verbosity, composeMessage)
import Handlers.Logger.Internal
import qualified System.IO as IO (Handle, hFlush, stdout)

withHandle :: Log.Verbosity -> (Handle -> IO ()) -> IO ()
withHandle v = bracket (new v) close

new :: Log.Verbosity -> IO Handle
new verbosity = new_ verbosity `catch` rethrow
  where
    rethrow = throwM . convert
    convert :: IOError -> AppError
    convert = loggerError . ("Logger Initiation Error: " <>) . T.tshow

new_ :: Log.Verbosity -> IO Handle
new_ v = do
  let hStdout = IO.stdout
  let getLog :: Log.Priority -> Text -> IO ()
      getLog p t = when (p >= v) (doLog_ hStdout p t) `catch` rethrow
  pure $ Handle {getLog}
  where
    rethrow = throwM . convert
    convert :: IOError -> AppError
    convert = loggerError . ("Logger failed to write message to StdOut: " <>) . T.tshow

doLog_ :: IO.Handle -> Log.Priority -> Text -> IO ()
doLog_ hStdout p t = Log.composeMessage p t >>= T.hPutStrLn hStdout

close :: Handle -> IO ()
close _hLog = IO.hFlush IO.stdout
