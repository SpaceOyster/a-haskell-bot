{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Logger where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Prelude hiding (log)
import qualified System.IO as IO

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

data Config =
    Config
        { fileM :: Maybe FilePath
        , verbosity :: Verbosity
        }


data Handle =
    Handle
        { logger :: Log
        , verbosity :: Verbosity
        }


log :: MonadIO m => Handle -> Verbosity -> String -> m ()
log Handle {..} v s =
    liftIO $
    if v >= verbosity
        then IO.hPutStrLn (getLogIO logger) s
        else noLog

noLog :: (Monad m) => m ()
noLog = pure ()

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
