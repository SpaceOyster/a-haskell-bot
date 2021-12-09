{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Logger
    ( Config(..)
    , Handle(..)
    , Priority(..)
    , HasLog(..)
    , withHandle
    , withHandlePure
    , logDebug
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (finally)
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
    ( FromJSON(..)
    , (.!=)
    , (.:?)
    , withObject
    , withText
    )
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import qualified Data.Text.Extended as T (Text, pack, toUpper, tshow, unpack)
import qualified Data.Text.IO as T (hPutStrLn)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Data.Time.LocalTime as Time (getZonedTime)
import Prelude hiding (log)
import qualified System.IO as IO
    ( FilePath
    , Handle
    , IOMode(..)
    , hFlush
    , stdout
    , withFile
    )

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

type Verbosity = Priority

prioToText :: Priority -> T.Text
prioToText = T.toUpper . T.tshow

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
            , verbosity = verbosity c0 `max` verbosity c1
            }

instance Monoid Config where
    mempty = Config {file = mempty, verbosity = Info}

instance A.FromJSON Config where
    parseJSON =
        A.withObject "FromJSON Logger.Config" $ \o -> do
            file <- o A..:? "file"
            verbosity <- o A..:? "verbosity" A..!= Info
            pure $ Config {..}

newtype Handle =
    Handle
        { log :: Priority -> T.Text -> IO ()
        }

class HasLog a where
    getLog :: a -> (Priority -> T.Text -> IO ())

instance HasLog (Priority -> T.Text -> IO ()) where
    getLog = id

instance HasLog Handle where
    getLog = log

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle Config {..} io =
    case file of
        Just f -> withFileLog verbosity f io
        Nothing -> withStdoutLog verbosity io

withFileLog :: Verbosity -> IO.FilePath -> (Handle -> IO ()) -> IO ()
withFileLog v path io = IO.withFile path IO.AppendMode (io <=< newFileLog v)

newFileLog :: Verbosity -> IO.Handle -> IO Handle
newFileLog v hFile = do
    mutex <- newMVar ()
    let doLog p t =
            withMVar mutex $ \() -> composeMessage p t >>= T.hPutStrLn hFile
    let log = \p t -> when (p >= v) $ doLog p t
    pure $ Handle {log}

withStdoutLog :: Verbosity -> (Handle -> IO ()) -> IO ()
withStdoutLog v io = do
    let hStdout = IO.stdout
    let log = \p t -> when (p >= v) $ composeMessage p t >>= T.hPutStrLn hStdout
    io Handle {log} `finally` IO.hFlush hStdout

withHandlePure :: Config -> (Handle -> IO a) -> IO (a, [(Priority, T.Text)])
withHandlePure Config {..} io = do
    logRef <- newIORef []
    let log p t =
            when (p >= verbosity) $
            atomicModifyIORef logRef $ \l -> ((p, t) : l, ())
    x <- io $ Handle {log}
    logMsgs <- readIORef logRef
    pure (x, reverse logMsgs)

composeMessage :: Priority -> T.Text -> IO T.Text
composeMessage p t = do
    ts <- timeStamp
    pure $ ts <> " " <> prioToText p <> " " <> t

timeStamp :: IO T.Text
timeStamp = do
    time <- Time.getZonedTime
    pure . T.pack $ Time.formatTime Time.defaultTimeLocale "%b %d %X %Z" time

logDebug, logInfo, logWarning, logError ::
       (MonadIO m, HasLog a) => a -> T.Text -> m ()
logDebug h = liftIO . getLog h Debug

logInfo h = liftIO . getLog h Info

logWarning h = liftIO . getLog h Warning

logError h = liftIO . getLog h Error
