{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Logger
    ( Config(..)
    , Handle(..)
    , Priority(..)
    , withHandle
    , log
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
    ( FromJSON(..)
    , (.!=)
    , (.:?)
    , withObject
    , withText
    )
import qualified Data.Char as Char (toUpper)
import Data.Maybe (maybe)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Data.Time.LocalTime as Time (getZonedTime)
import Prelude hiding (log)
import qualified System.IO as IO
    ( Handle
    , IOMode(..)
    , hClose
    , hFlush
    , openFile
    , stdout
    )

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

type Verbosity = Priority

prioToText :: Priority -> T.Text
prioToText = T.pack . fmap Char.toUpper . show

instance A.FromJSON Priority where
    parseJSON =
        A.withText "FromJSON Logger.Priority" $ \t ->
            case t of
                "debug" -> pure Debug
                "info" -> pure Info
                "warning" -> pure Warning
                "error" -> pure Error
                _ -> fail $ "Unknown verbosity: " ++ T.unpack t

data Config =
    Config
        { file :: Maybe FilePath
        , verbosity :: Verbosity
        }
    deriving (Show)

instance Semigroup Config where
    c0 <> c1 =
        Config
            { file = file c0 <|> file c1
            , verbosity =
                  verbosity (c0 :: Config) `max` verbosity (c1 :: Config)
            }

instance Monoid Config where
    mempty = Config {file = mempty, verbosity = Info}

instance A.FromJSON Config where
    parseJSON =
        A.withObject "FromJSON Logger.Config" $ \o -> do
            file <- o A..:? "file"
            verbosity <- o A..:? "verbosity" A..!= Info
            pure $ Config {..}

data Handle =
    Handle
        { logger :: Log
        , verbosity :: Priority
        }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle Config {..} f =
    bracket
        (newLogger . maybe LogTypeStdout LogTypeFile $ file)
        closeLogger
        (\logger -> do
             let hLog = Handle {..}
             logInfo hLog "Logger initiated"
             f hLog)

log :: MonadIO m => Handle -> Priority -> T.Text -> m ()
log Handle {..} v s =
    liftIO $ do
        ts <- timeStamp
        if v >= verbosity
            then T.hPutStrLn (getLogIO logger) $
                 ts <> " " <> prioToText v <> " " <> s
            else noLog

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

logDebug :: MonadIO m => Handle -> T.Text -> m ()
logDebug h = log h Debug

logInfo :: MonadIO m => Handle -> T.Text -> m ()
logInfo h = log h Info

logWarning :: MonadIO m => Handle -> T.Text -> m ()
logWarning h = log h Warning

logError :: MonadIO m => Handle -> T.Text -> m ()
logError h = log h Error
