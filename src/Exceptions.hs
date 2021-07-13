module Exceptions where

-- | Suggested module import
-- @
-- import qualified Exceptions as Priority
-- import Exceptions (BotException(..))
-- @
import Control.Monad.Catch (Exception(..))
import Data.Typeable (Typeable)

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Enum, Show)

data BotException
    = Ex Priority !String
    | URLParsing !String
    deriving (Typeable)

instance Exception BotException

instance Show BotException where
    show (Ex priority description) = "[" ++ show priority ++ "] " ++ description
    show (URLParsing description) = "URLParsing: " <> description
