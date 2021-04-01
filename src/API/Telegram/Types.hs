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
