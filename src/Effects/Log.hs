{-# LANGUAGE OverloadedStrings #-}

module Effects.Log where

import qualified Data.Aeson as A
  ( FromJSON(..)
  , (.!=)
  , (.:?)
  , withObject
  , withText
  )
import qualified Data.Text.Extended as T (Text, pack, toUpper, tshow, unpack)

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

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

class Monad m =>
      MonadLog m
  where
  doLog :: Priority -> T.Text -> m ()

logDebug, logInfo, logWarning, logError :: MonadLog m => T.Text -> m ()
logDebug = doLog Debug

logInfo = doLog Info

logWarning = doLog Warning

logError = doLog Error
