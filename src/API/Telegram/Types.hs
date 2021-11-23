{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module API.Telegram.Types where

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , (.:?)
  , decode
  , defaultOptions
  , encode
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , withObject
  )
import Data.Aeson.Types (Value)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import qualified Data.Hashable as H
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))
import GHC.Generics

data Update =
  Update
    { update_id :: Integer
    , message :: Maybe Message
    , callback_query :: Maybe CallbackQuery
    }
  deriving (Show, Generic, FromJSON)

getMessageThrow :: (MonadThrow m) => Update -> m Message
getMessageThrow Update {update_id, message} =
  case message of
    Just t -> pure t
    Nothing ->
      throwM $
      Ex Priority.Info $ "Update " ++ show update_id ++ " has no message"

data Message =
  Message
    { message_id :: Integer
    , from :: Maybe User
    , chat :: Chat
    , date :: Integer
    , text :: Maybe String
    }
  deriving (Show, Generic, FromJSON)

getAuthorThrow :: (MonadThrow m) => Message -> m User
getAuthorThrow Message {message_id, from} =
  case from of
    Just t -> pure t
    Nothing ->
      throwM $
      Ex Priority.Info $ "Message " ++ show message_id ++ " has no author"

getTextThrow :: (MonadThrow m) => Message -> m String
getTextThrow Message {message_id, text} =
  case text of
    Just t -> pure t
    Nothing ->
      throwM $
      Ex Priority.Info $ "Message " ++ show message_id ++ " has no text"

data BotCommand =
  BotCommand
    { command :: String
    , description :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)

data CallbackQuery =
  CallbackQuery
    { id :: String
    , from :: User
    , message :: Maybe Message
    , inline_message_id :: Maybe String
    , query_data :: Maybe String
    }
  deriving (Show)

getQDataThrow :: (MonadThrow m) => CallbackQuery -> m String
getQDataThrow CallbackQuery {id, query_data} =
  case query_data of
    Just d -> pure d
    Nothing ->
      throwM $
      Ex Priority.Info $ "CallbackQuery " ++ show id ++ " has no query data"

instance FromJSON CallbackQuery where
  parseJSON =
    withObject "CallbackQuery" $ \o -> do
      id <- o .: "id"
      from <- o .: "from"
      message <- o .:? "message"
      inline_message_id <- o .:? "inline_message_id"
      query_data <- o .:? "data"
      pure $ CallbackQuery {..}

newtype User =
  User
    { user_id :: Integer
    }
  deriving (Show, Generic)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 5}

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 5}

instance H.Hashable User where
  hash = user_id

newtype Chat =
  Chat
    { chat_id :: Integer
    }
  deriving (Show)

instance FromJSON Chat where
  parseJSON = withObject "Chat" (fmap Chat . (.: "id"))

newtype InlineKeyboardMarkup =
  InlineKeyboardMarkup
    { inline_keyboard :: [[InlineKeyboardButton]]
    }
  deriving (Show, Generic, ToJSON)

data InlineKeyboardButton =
  InlineKeyboardButton
    { text :: String
    , callback_data :: String
    }
  deriving (Show, Generic, ToJSON)

data Response
  = Error
      { error_code :: Int
      , description :: String
      }
  | Updates [Update]
  | OtherResponse Value
  deriving (Show)
      -- ^actually either: 
      -- 1. [Update] - for `getUpdates`
      -- 2. Message - for `sendMessage` and `sendInlineKeyboard`
      -- 3. True - for `answerCallbackQuery`
      -- 4. `{"message_id": 12345}` - for `copyMessage`

instance FromJSON Response where
  parseJSON =
    withObject "Response" $ \o -> do
      ok <- o .: "ok"
      if ok
        then Updates <$> o .: "result" <|> OtherResponse <$> o .: "result"
        else Error <$> o .: "error_code" <*> o .: "description"

-- | Gets @[Updates]@ from @Respose@ if it was successful or throws error
-- otherwise
extractUpdates :: (MonadThrow m) => Response -> m [Update]
extractUpdates res =
  case res of
    Error {error_code, description} ->
      throwM $ Ex Priority.Warning (show error_code ++ ": " ++ description)
    Updates us -> pure us
    x -> pure []
