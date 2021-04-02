{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module API.Telegram.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), decode, encode, withObject)
import GHC.Generics

data Status =
    Status
        { ok :: Bool
        , error_code :: Int
        , description :: String
        }
    deriving (Show, Generic, ToJSON, FromJSON)

data Response
    = Error
          { error_code :: Int
          , description :: String
          }
    | Result
          { result :: [String]
          }
    deriving (Show)

instance FromJSON Response where
    parseJSON =
        withObject "Response" $ \o -> do
            ok <- o .: "ok"
            if ok
                then Result <$> o .: "result"
                else Error <$> o .: "error_code" <*> o .: "description"
