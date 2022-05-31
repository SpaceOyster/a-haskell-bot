module Test.App.Error
  ( anyAPIError,
    anyAppError,
    anyBotError,
    anyDBError,
    anyHTTPError,
    anyLoggerError,
  )
where

import App.Error as App (AppError (..))
import Test.Hspec (Selector)

anyAppError :: Selector App.AppError
anyAppError = const True

anyAPIError :: Selector App.AppError
anyAPIError (App.APIError _) = True
anyAPIError _ = False

anyBotError :: Selector App.AppError
anyBotError (App.BotError _) = True
anyBotError _ = False

anyHTTPError :: Selector App.AppError
anyHTTPError (App.HTTPError _) = True
anyHTTPError _ = False

anyLoggerError :: Selector App.AppError
anyLoggerError (App.LoggerError _) = True
anyLoggerError _ = False

anyDBError :: Selector App.AppError
anyDBError (App.DBError _) = True
anyDBError _ = False
