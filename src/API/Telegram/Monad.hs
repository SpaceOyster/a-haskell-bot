{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Telegram.Monad where

import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.State (MonadState (..), StateT, get, put)
import Control.Monad.Trans (MonadTrans (..), lift)
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
      MonadState TGState
    )

instance MonadTrans TelegramT where
  lift = TelegramT . lift

newtype Config = Config
  { key :: String
  }

instance Semigroup TGState where
  _a <> b = b

instance Monoid TGState where
  mempty = TGState {lastUpdate = 0, apiURI = URI.nullURI}
