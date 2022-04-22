{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Telegram.Monad where

import API.Telegram.Types (Update (update_id))
import App.Error (apiError)
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.State (MonadState (..), StateT, get, put)
import Control.Monad.Trans (MonadTrans (..), lift)
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

data TGState = TGState
  { lastUpdate :: Integer,
    apiURI :: URI.URI
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

instance (Log.MonadLog m) => Log.MonadLog (TelegramT m) where
  doLog p t = lift . Log.doLog p $ "[Telegram] " <> t

newtype Config = Config
  { key :: String
  }

emptyTGState :: TGState
emptyTGState = TGState {lastUpdate = 0, apiURI = URI.nullURI}

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
  Log.logInfo "Initiating Telegram API handle"
  apiURI <- makeBaseURI cfg
  pure $ emptyTGState {apiURI}

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ apiError "Unable to parse Telegram API URL"
