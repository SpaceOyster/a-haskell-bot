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


data LogType a where
    LogFile' :: FilePath -> LogType IO.Handle
    LogStdout' :: LogType IO.Handle
