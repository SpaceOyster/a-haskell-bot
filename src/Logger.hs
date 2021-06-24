{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Logger where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Char as Char (toUpper)
import Data.Maybe (maybe)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Data.Time.LocalTime as Time (getZonedTime)
import Prelude hiding (error, log)
import qualified System.IO as IO

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

verbToText :: Verbosity -> T.Text
verbToText = T.pack . fmap Char.toUpper . show

data Config =
    Config
        { fileM :: Maybe FilePath
        , verbosity :: Verbosity
        }

instance Semigroup Config where
    c0 <> c1 =
        Config
            { fileM = fileM c0 <|> fileM c1
            , verbosity =
                  verbosity (c0 :: Config) `max` verbosity (c1 :: Config)
            }

instance Monoid Config where
    mempty = Config {fileM = mempty, verbosity = Debug}

data Handle =
    Handle
        { logger :: Log
        , verbosity :: Verbosity
        }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle Config {..} f =
    bracket
        (newLogger . maybe LogTypeStdout LogTypeFile $ fileM)
        closeLogger
        (\logger -> f (Handle {..}))

log :: MonadIO m => Handle -> Verbosity -> T.Text -> m ()
log Handle {..} v s =
    liftIO $ do
        ts <- timeStamp
        if v >= verbosity
            then T.hPutStrLn (getLogIO logger) $
                 ts <> " " <> verbToText v <> " " <> s
            else noLog

log' :: MonadIO m => Handle -> Verbosity -> String -> m ()
log' hLog v = log hLog v . T.pack

noLog :: (Monad m) => m ()
noLog = pure ()

data LogType
    = LogTypeFile FilePath
    | LogTypeStdout

data Log
    = LogFile IO.Handle
    | LogStdout IO.Handle

newLogger :: LogType -> IO Log
newLogger ltype =
    case ltype of
        LogTypeStdout -> pure $ LogStdout IO.stdout
        LogTypeFile path -> LogFile <$> IO.openFile path IO.AppendMode

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

timeStamp :: IO T.Text
timeStamp = do
    time <- Time.getZonedTime
    pure . T.pack $ Time.formatTime Time.defaultTimeLocale "%b %d %X %Z" time

debug :: MonadIO m => Handle -> T.Text -> m ()
debug h = log h Debug

info :: MonadIO m => Handle -> T.Text -> m ()
info h = log h Info

warning :: MonadIO m => Handle -> T.Text -> m ()
warning h = log h Warning

error :: MonadIO m => Handle -> T.Text -> m ()
error h = log h Error

debug' :: MonadIO m => Handle -> String -> m ()
debug' h = log h Debug . T.pack

info' :: MonadIO m => Handle -> String -> m ()
info' h = log h Info . T.pack

warning' :: MonadIO m => Handle -> String -> m ()
warning' h = log h Warning . T.pack

error' :: MonadIO m => Handle -> String -> m ()
error' h = log h Error . T.pack
