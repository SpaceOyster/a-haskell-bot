module Data.Aeson.Extended
    ( module Data.Aeson
    , throwDecode
    ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson
import Data.ByteString.Lazy as LBS (ByteString)
import Exceptions (ParsingException(..))

throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode lbs =
    case eitherDecode lbs of
        Left err -> throwM $ ParsingException err
        Right a -> pure a
