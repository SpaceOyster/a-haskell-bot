{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API.Telegram.Types where

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), decode, encode, withObject)
import Data.Aeson.Types (Value)
import Data.Function ((&))
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))
import GHC.Generics
import Utils

data Update =
  Update
    { update_id :: Integer
    , message :: Message -- TODO it's actually optional
    }
  deriving (Show, Generic, FromJSON)

data Message =
  Message
    { message_id :: Integer
    , from :: Maybe User
    , chat :: Chat
    , date :: Integer
    , text :: Maybe String
    }
  deriving (Show, Generic, FromJSON)

getTextThrow :: (MonadThrow m) => Message -> m String
getTextThrow Message {message_id, text} =
  case text of
    Just t -> return t
    Nothing ->
      throwM $
      Ex Priority.Info $ "Message " ++ show message_id ++ " has no text"

isCommand :: String -> Bool
isCommand "" = False
isCommand s = (== '/') . head $ s

getCommandThrow :: (MonadThrow m) => Message -> m String
getCommandThrow msg@Message {message_id} = do
  t <- getTextThrow msg
  unless (isCommand t) $
    throwM $
    Ex Priority.Info $ "Message " ++ show message_id ++ " is not a command"
  return . takeWhile (/= ' ') . tail $ t

data BotCommand =
  BotCommand
    { command :: String
    , description :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)

data MessageCopy =
  MessageCopy
    { chat_id :: Integer
    , from_chat_id :: Integer
    , message_id :: Integer
    }
  deriving (Show, Generic, ToJSON)

copyMessage :: Message -> MessageCopy
copyMessage Message {message_id, chat} =
  let chId = chat_id (chat :: Chat)
   in MessageCopy {chat_id = chId, from_chat_id = chId, message_id = message_id}

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

-- | Gets @[Updates]@ from @Respose@ if it was successful or throws error
-- otherwise
getResult :: (MonadThrow m) => Response -> m [Update]
getResult res =
  case res of
    Error {error_code, description} ->
      throwM $ Ex Priority.Warning (show error_code ++ ": " ++ description)
    Result {result} -> return result
