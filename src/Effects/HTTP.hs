module Effects.HTTP where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.URI.Extended as URI

data Request
  = GET URI.URI
  | POST URI.URI L8.ByteString
  deriving (Show)

class Monad m =>
      MonadHTTP m
  where
  sendRequest :: Request -> m L8.ByteString
