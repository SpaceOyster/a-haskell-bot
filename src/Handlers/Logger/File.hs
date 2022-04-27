{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Logger.File
  ( module Handlers.Logger.Internal,
    withHandle,
    new,
  )
where

import App.Error (AppError, loggerError)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (when, (>=>))
import Control.Monad.Catch (catch, throwM)
import Data.Text.Extended as T (Text, tshow)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Effects.Log as Log (Priority, Verbosity, composeMessage)
import Handlers.Logger.Internal
import qualified System.IO as IO
  ( FilePath,
    Handle,
    IOMode (..),
    withFile,
  )

withHandle :: Log.Verbosity -> IO.FilePath -> (Handle -> IO ()) -> IO ()
withHandle v path io = IO.withFile path IO.AppendMode (new v >=> io)

new :: Log.Verbosity -> IO.Handle -> IO Handle
new verbosity hFile = new_ verbosity hFile `catch` rethrow
  where
    rethrow = throwM . convert
    convert :: IOError -> AppError
    convert = loggerError . ("Logger Initiation Error: " <>) . T.tshow

new_ :: Log.Verbosity -> IO.Handle -> IO Handle
new_ verbosity hFile = do
  mutex <- newMVar ()
  let getLog :: Log.Priority -> Text -> IO ()
      getLog p t = getLog_ verbosity mutex hFile p t `catch` rethrow
  pure $ Handle {getLog}
  where
    rethrow = throwM . convert
    convert :: IOError -> AppError
    convert = loggerError . ("Logger failed to append to file: " <>) . T.tshow

doLog_ :: MVar () -> IO.Handle -> Log.Priority -> Text -> IO ()
doLog_ mutex hFile priority t =
  withMVar mutex $ \_ -> Log.composeMessage priority t >>= T.hPutStrLn hFile

getLog_ :: Log.Verbosity -> MVar () -> IO.Handle -> Log.Priority -> Text -> IO ()
getLog_ verbosity mutex hFile p t =
  when (p >= verbosity) (doLog_ mutex hFile p t)
