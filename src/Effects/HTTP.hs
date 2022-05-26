{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.HTTP where

import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.URI.Extended as URI

data Request
  = GET URI.URI
  | POST URI.URI L8.ByteString
  deriving (Show, Eq)

class Monad m => MonadHTTP m where
  sendRequest :: Request -> m L8.ByteString

instance
  {-# OVERLAPPABLE #-}
  (MonadHTTP m, MonadTrans t, Monad (t m)) =>
  MonadHTTP (t m)
  where
  sendRequest = lift . sendRequest
