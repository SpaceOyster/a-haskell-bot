module API.Telegram.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), decode, encode, withObject)

data Status =
    Status
        { ok :: Bool
        , error_code :: Int
        , description :: String
        }
    deriving (Show)

instance FromJSON Status where
    parseJSON =
        withObject "error" $ \o -> do
            ok' <- o .: "ok"
            error_code' <- o .: "error_code"
            description' <- o .: "description"
            return $ Status ok' error_code' description'
