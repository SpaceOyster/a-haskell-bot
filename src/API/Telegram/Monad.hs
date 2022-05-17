{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Telegram.Monad where

import API.Telegram.Types (Update (update_id))
import App.Error (AppError, apiError)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans)
import qualified Data.Text.Extended as T (tshow)
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

data TGState = TGState
  { lastUpdate :: Integer,
    apiURI :: URI.URI,
    timeout :: Int
  }

class (Monad m) => MonadTelegram m where
  getTGState :: m TGState
  modifyTGState :: (TGState -> TGState) -> m ()
  modifyTGState f = getTGState >>= putTGState . f
  putTGState :: TGState -> m ()
  putTGState = modifyTGState . const

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

instance (Monad m) => MonadTelegram (TelegramT m) where
  getTGState = get
  putTGState = put

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
  (MonadThrow m, Log.MonadLog m, MonadTelegram m) => [Update] -> m [Update]
rememberLastUpdate us = do
  st <- getTGState
  mapM_ putTGState (newStateFromM us st) >> pure us

evalTelegramT ::
  (Monad m, MonadCatch m, Log.MonadLog m) =>
  Config ->
  TelegramT m a ->
  m a
evalTelegramT cfg t = do
  st <- initiate cfg
  evalStateT (unTelegramT t) st

initiate :: (MonadCatch m, Log.MonadLog m) => Config -> m TGState
initiate cfg = do
  Log.logInfo "Initiating Telegram Bot"
  Log.logDebug $ "Telegram Bot config: " <> T.tshow cfg
  initiate_ cfg `catch` \e -> do
    Log.logError "Failed to initiate Telegram Poll API"
    throwM (e :: AppError)

initiate_ :: (MonadThrow m, Log.MonadLog m) => Config -> m TGState
initiate_ cfg = do
  Log.logInfo $ "Initiating Telegram API handle with: " <> T.tshow cfg
  apiURI <- makeBaseURI cfg
  let timeout = min 100 (max 1 $ timeout_seconds cfg)
  pure $ emptyTGState {apiURI, timeout}

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {key} =
  maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ apiError "Unable to parse Telegram API URL"

apiMethod :: MonadTelegram m => String -> m URI.URI
apiMethod method = do
  st <- getTGState
  pure $ apiURI st `URI.addPath` method
