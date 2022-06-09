{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Telegram.Types
  ( CallbackQuery (..),
    Error (..),
    Response (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    Chat (..),
    User (..),
    Message (..),
    Update (..),
    fromResponse,
  )
where

import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    encode,
    fromJSON,
    genericParseJSON,
    genericToJSON,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson.Types as A
  ( Options (fieldLabelModifier),
    Result (..),
    Value (Object),
    defaultOptions,
  )
import qualified Data.Hashable as H
import qualified Data.Text as T
import qualified Data.Text.Extended as T
import GHC.Generics (Generic)

data Message = Message
  { message_id :: Integer,
    from :: Maybe User,
    chat :: Chat,
    date :: Integer,
    text :: Maybe T.Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data BotCommand = BotCommand
  { command :: T.Text,
    description :: T.Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)

data CallbackQuery = CallbackQuery
  { cq_id :: T.Text,
    from :: User,
    message :: Maybe Message,
    inline_message_id :: Maybe T.Text,
    query_data :: Maybe T.Text
  }
  deriving (Eq, Show)

instance FromJSON CallbackQuery where
  parseJSON =
    withObject "CallbackQuery" $ \o -> do
      cq_id <- o .: "id"
      from <- o .: "from"
      message <- o .:? "message"
      inline_message_id <- o .:? "inline_message_id"
      query_data <- o .:? "data"
      pure $ CallbackQuery {..}

instance ToJSON CallbackQuery where
  toJSON CallbackQuery {..} =
    object
      [ "id" .= cq_id,
        "from" .= from,
        "message" .= message,
        "inline_message_id" .= inline_message_id,
        "data" .= query_data
      ]

newtype User = User
  { user_id :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 5}

instance FromJSON User where
  parseJSON = genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 5}

instance H.Hashable User where
  hash = user_id

newtype Chat = Chat
  { chat_id :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Chat where
  parseJSON = withObject "Chat" (fmap Chat . (.: "id"))

instance ToJSON Chat where
  toJSON = genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 5}

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inline_keyboard :: [[InlineKeyboardButton]]
  }
  deriving (Show, Generic, ToJSON)

data InlineKeyboardButton = InlineKeyboardButton
  { text :: T.Text,
    callback_data :: T.Text
  }
  deriving (Show, Generic, ToJSON)

data Update = Update
  { update_id :: Integer,
    message :: Maybe Message,
    callback_query :: Maybe CallbackQuery
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

data Error = Error
  { error_code :: Int,
    description :: T.Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Response
  = ErrorResponse Error
  | ResponseWith A.Value
  deriving (Show)

instance FromJSON Response where
  parseJSON =
    withObject "Response" $ \o -> do
      ok <- o .: "ok"
      if not ok
        then ErrorResponse <$> parseJSON (A.Object o)
        else ResponseWith <$> o .: "result"

instance ToJSON Response where
  toJSON (ErrorResponse Error {..}) =
    object
      [ "ok" .= False,
        "error_code" .= error_code,
        "description" .= description
      ]
  toJSON (ResponseWith v) = object ["ok" .= True, "result" .= v]

fromResponse :: (FromJSON a, MonadThrow m) => Response -> m a
fromResponse (ErrorResponse err) =
  throwM . apiError $
    "Responded with Error: "
      <> T.tshow (error_code err)
      <> ": "
      <> description (err :: Error)
fromResponse (ResponseWith v) =
  case fromJSON v of
    A.Success a -> pure a
    A.Error _ ->
      throwM . apiError $
        "Responded with unexpected result: " <> T.lazyDecodeUtf8 (encode v)
