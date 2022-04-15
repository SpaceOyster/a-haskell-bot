{-# LANGUAGE DeriveAnyClass #-}

module App.Error where

import Control.Monad.Catch (Exception)
import Data.Text

data AppError
  = APIError Text
  | BotError Text
  | HTTPError Text
  | LoggerError Text
  | DBError Text
  deriving (Show, Exception)

apiError :: Text -> AppError
apiError = APIError

botError :: Text -> AppError
botError = BotError

httpError :: Text -> AppError
httpError = HTTPError

loggerError :: Text -> AppError
loggerError = LoggerError

dbError :: Text -> AppError
dbError = DBError
