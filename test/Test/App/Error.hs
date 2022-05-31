module Test.App.Error
  ( anyAppError,
    isAPIError,
    isBotError,
    isDBError,
    isHTTPError,
    isLoggerError,
  )
where

import App.Error as App (AppError (..))
import Test.Hspec (Selector)

anyAppError :: Selector App.AppError
anyAppError = const True

isAPIError :: Selector App.AppError
isAPIError (App.APIError _) = True
isAPIError _ = False

isBotError :: Selector App.AppError
isBotError (App.BotError _) = True
isBotError _ = False

isHTTPError :: Selector App.AppError
isHTTPError (App.HTTPError _) = True
isHTTPError _ = False

isLoggerError :: Selector App.AppError
isLoggerError (App.LoggerError _) = True
isLoggerError _ = False

isDBError :: Selector App.AppError
isDBError (App.DBError _) = True
isDBError _ = False
