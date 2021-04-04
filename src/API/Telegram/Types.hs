{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module API.Telegram.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), decode, encode, withObject)
import GHC.Generics

data Update =
  Update
    { update_id :: Integer
    , message :: Maybe Message
    }
  deriving (Show, Generic, FromJSON)

data Message =
  Message
    { message_id :: Integer
    , from :: Maybe User
    , chat :: Maybe Chat
    , date :: Integer
    , text :: Maybe String
    }
  deriving (Show, Generic, FromJSON)

data User =
  User
    { id :: Integer
    }
  deriving (Show, Generic, FromJSON)

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
