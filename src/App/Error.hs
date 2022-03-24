module App.Error where

import Control.Monad.Catch (Exception (..))
import Data.Text.Extended

data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Enum, Show)

data AppError
  = NotFound
  | APIError Text
  | BotError Text
  | HTTPError Text
  deriving (Show)

instance Exception AppError

apiError :: Text -> AppError
apiError = APIError

botError :: Text -> AppError
botError = BotError

httpError :: Text -> AppError
httpError = HTTPError
