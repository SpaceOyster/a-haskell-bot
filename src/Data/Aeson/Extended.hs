module Data.Aeson.Extended
    ( module Data.Aeson
    , throwDecode
    , parseThrow
    ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy as LBS (ByteString)
import Exceptions (ParsingException(..))

throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode lbs =
    case eitherDecode lbs of
        Left err -> throwM $ ParsingException err
        Right a -> pure a

parseThrow :: (MonadThrow m, FromJSON a) => (a -> Parser b) -> a -> m b
parseThrow p x =
    case parseEither p x of
        Left err -> throwM $ ParsingException err
        Right b -> pure b
