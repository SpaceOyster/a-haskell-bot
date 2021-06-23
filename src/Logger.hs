{-# LANGUAGE GADTs #-}
module Logger where

import Prelude hiding (log)
import qualified System.IO as IO
data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)


data Handle =
    Handle
        { logger :: Log
        , verbosity :: Verbosity
        }

data LogType a where
    LogFile' :: FilePath -> LogType IO.Handle
    LogStdout' :: LogType IO.Handle

data Log
    = LogFile IO.Handle
    | LogStdout IO.Handle

newLogger :: LogType IO.Handle -> IO Log
newLogger ltype =
    case ltype of
        LogStdout' -> pure $ LogStdout IO.stdout
        LogFile' path -> LogFile <$> IO.openFile path IO.AppendMode

closeLogger :: Log -> IO ()
closeLogger ltype =
    case ltype of
        LogStdout hStdout -> IO.hFlush hStdout
        LogFile hFile -> IO.hClose hFile

getLogIO :: Log -> IO.Handle
getLogIO l =
    case l of
        LogFile h -> h
        LogStdout h -> h
