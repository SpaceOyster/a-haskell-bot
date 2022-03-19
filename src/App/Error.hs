module App.Error where

import Control.Monad.Catch (Exception(..))
import Data.Typeable (Typeable)

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Enum, Show)

data BotException
    = Ex !String
    | URLParsing !String
    deriving (Typeable)

instance Exception BotException

instance Show BotException where
    show (Ex description) =  "Ex: " <> description
    show (URLParsing description) = "URLParsing: " <> description

data APIException
    = APIStateSetting !String
    | APIRespondedWithError !String
    | VKPollError !String

instance Exception APIException

instance Show APIException where
    show (APIStateSetting description) = "APIStateSetting: " <> description
    show (APIRespondedWithError description) =
        "APIRespondedWithError: " <> description
    show (VKPollError c) = "VKPollError: Code " <> c

data ParsingException =
    ParsingException !String

instance Exception ParsingException

instance Show ParsingException where
    show (ParsingException description) = "ParsingException: " <> description
