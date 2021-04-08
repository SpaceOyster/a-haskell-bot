module Utils
    ( throwDecode
    ) where

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON(..), eitherDecode)
import Data.ByteString.Lazy as LBS
import qualified Exceptions as Priority
import Exceptions (BotException(..))

throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode lbs =
    case eitherDecode lbs of
        Left err -> throwM $ Ex Priority.Warning err
        Right a -> return a
