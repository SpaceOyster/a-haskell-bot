{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API.Telegram.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), decode, encode, withObject)
import Data.Aeson.Types (Value)
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

data MessageCopy =
  MessageCopy
    { chat_id :: Integer
    , from_chat_id :: Integer
    , message_id :: Integer
    }
  deriving (Show, Generic, ToJSON)

data User =
  User
    { id :: Integer
    }
  deriving (Show, Generic, FromJSON)

data Chat =
  Chat
    { chat_id :: Integer
    }
  deriving (Show)

instance FromJSON Chat where
  parseJSON = withObject "Chat" (fmap Chat . (.: "id"))

data Response
  = Error
      { error_code :: Int
      , description :: String
      }
  | Result
      { result :: [Update]
      }
  deriving (Show)

instance FromJSON Response where
  parseJSON =
    withObject "Response" $ \o -> do
      ok <- o .: "ok"
      if ok
        then Result <$> o .: "result"
        else Error <$> o .: "error_code" <*> o .: "description"
