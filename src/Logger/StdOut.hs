{-# LANGUAGE NamedFieldPuns #-}

module Logger.StdOut (module Logger.Internal, withHandle, new, close) where

import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Effects.Log as Log (Verbosity, composeMessage)
import Logger.Internal
import qualified System.IO as IO (hFlush, stdout)

withHandle :: Log.Verbosity -> (Handle -> IO ()) -> IO ()
withHandle v = bracket (new v) close

new :: Log.Verbosity -> IO Handle
new v = do
  let hStdout = IO.stdout
  let getLog =
        \p t -> when (p >= v) $ Log.composeMessage p t >>= T.hPutStrLn hStdout
  pure $ Handle {getLog}

close :: Handle -> IO ()
close _hLog = IO.hFlush IO.stdout
