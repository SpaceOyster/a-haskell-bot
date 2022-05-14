{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Telegram.Monad where

import API.Telegram.Types (Update (update_id))
import App.Error (apiError)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.State (MonadState, StateT, get, put)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Text.Extended as T (tshow)
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

data TGState = TGState
  { lastUpdate :: Integer,
    apiURI :: URI.URI,
    timeout :: Int
  }

newtype TelegramT m a = TelegramT {unTelegramT :: StateT TGState m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadState TGState,
      MonadTrans
    )

class (Monad m) => MonadTelegram m where
  getTGState :: m TGState
  modifyTGState :: (TGState -> TGState) -> m ()
  modifyTGState f = getTGState >>= putTGState . f
  putTGState :: TGState -> m ()
  putTGState = modifyTGState . const

instance (Monad m) => MonadTelegram (TelegramT m) where
  getTGState = get
  putTGState = put

instance (Log.MonadLog m) => Log.MonadLog (TelegramT m) where
  doLog priority text = lift . Log.doLog priority $ "[Telegram] " <> text

data Config = Config
  { key :: String,
    timeout_seconds :: Int
  }
  deriving (Show)

emptyTGState :: TGState
emptyTGState = TGState {lastUpdate = 0, apiURI = URI.nullURI, timeout = 100}

newStateFromM :: [Update] -> TGState -> Maybe TGState
newStateFromM us@(_ : _) st = Just $ st {lastUpdate = 1 + update_id (last us)}
newStateFromM _ _ = Nothing

rememberLastUpdate ::
  (MonadThrow m, Log.MonadLog m) => [Update] -> TelegramT m [Update]
rememberLastUpdate us = do
  st <- get
  mapM_ put (newStateFromM us st) >> pure us

initiate :: (MonadThrow m, Log.MonadLog m) => Config -> m TGState
initiate cfg = do
  Log.logInfo $ "Initiating Telegram API handle with: " <> T.tshow cfg
  apiURI <- makeBaseURI cfg
  let timeout = min 100 (max 1 $ timeout_seconds cfg)
  pure $ emptyTGState {apiURI, timeout}

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ apiError "Unable to parse Telegram API URL"

apiMethod :: Monad m => String -> TelegramT m URI.URI
apiMethod method = do
  st <- get
  pure $ apiURI st `URI.addPath` method
