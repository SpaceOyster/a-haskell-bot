{-# LANGUAGE NamedFieldPuns #-}

module Logger.File (module Logger.Internal, withHandle, new) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Monad (when, (<=<))
import Data.Text (Text)
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
new verbosity hFile = do
  mutex <- newMVar ()
  let doLog :: Log.Priority -> Text -> IO ()
      doLog priority t =
        withMVar mutex $ \_ -> Log.composeMessage priority t >>= T.hPutStrLn hFile
  let getLog :: Log.Priority -> Text -> IO ()
      getLog = \priority t -> when (priority >= verbosity) $ doLog priority t
  pure $ Handle {getLog}
