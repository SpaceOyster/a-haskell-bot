{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Log where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Aeson as A (FromJSON (..), withText)
import qualified Data.Text.Extended as T (Text, pack, unpack)
import qualified Data.Time.Format as Time (defaultTimeLocale, formatTime)
import qualified Data.Time.LocalTime as Time (getZonedTime)

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

type Verbosity = Priority

priorityToText :: Priority -> T.Text
priorityToText Debug = "DEBUG"
priorityToText Info = "INFO"
priorityToText Warning = "WARNING"
priorityToText Error = "ERROR"

instance A.FromJSON Priority where
  parseJSON =
    A.withText "FromJSON Logger.Priority" $ \t ->
      case t of
        "debug" -> pure Debug
        "info" -> pure Info
        "warning" -> pure Warning
        "error" -> pure Error
        _ -> fail $ "Unknown verbosity: " <> T.unpack t

class
  Monad m =>
  MonadLog m
  where
  doLog :: Priority -> T.Text -> m ()

instance
  {-# OVERLAPPABLE #-}
  (MonadLog m, MonadTrans t, Monad (t m)) =>
  MonadLog (t m)
  where
  doLog p t = lift $ doLog p t

timeStamp :: IO T.Text
timeStamp = do
  time <- Time.getZonedTime
  pure . T.pack $ Time.formatTime Time.defaultTimeLocale "%b %d %X %Z" time

composeMessage :: (MonadIO m) => Priority -> T.Text -> m T.Text
composeMessage p t = do
  ts <- liftIO timeStamp
  pure $ ts <> " " <> priorityToText p <> " " <> t

logDebug, logInfo, logWarning, logError :: MonadLog m => T.Text -> m ()
logDebug = doLog Debug
logInfo = doLog Info
logWarning = doLog Warning
logError = doLog Error
