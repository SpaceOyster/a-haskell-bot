module Exceptions where

-- | Suggested module import
-- @
-- import qualified Exceptions as Priority
-- import Exceptions (BotException(..))
-- @
import Control.Monad.Catch (Exception(..))

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Show)

data BotException =
    Ex Priority String
    deriving (Show)

instance Exception BotException
