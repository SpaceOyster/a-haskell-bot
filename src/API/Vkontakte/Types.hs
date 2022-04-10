{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Vkontakte.Types where

import App.Error (apiError)
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.Aeson as A
import Data.Char (toLower)
import Data.Foldable (asum)
import qualified Data.Hashable as H
import qualified Data.Text.Extended as T
import GHC.Generics (Generic)
import qualified Network.URI.Extended as URI

newtype User = User
  { unUser :: Integer
  }
  deriving (Show)

instance H.Hashable User where
  hash = unUser

data Message = Message
  { msg_id :: Integer,
    msg_date :: Integer,
    msg_peer_id :: Integer,
    msg_from_id :: Integer,
    msg_text :: T.Text,
    msg_random_id :: Maybe Integer,
    msg_attachments :: [Attachment],
    msg_payload :: Maybe A.Value,
    msg_keyboard :: Maybe Keyboard,
    msg_is_cropped :: Maybe Bool
  }
  deriving (Show, Generic)

instance A.FromJSON Message where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 4}

data MediaDoc = MediaDoc
  { mdoc_id :: Integer,
    mdoc_owner_id :: Integer,
    mdoc_access_key :: Maybe T.Text
  }
  deriving (Show, Generic)

instance A.FromJSON MediaDoc where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 5}

data Sticker = Sticker
  { product_id :: Integer,
    sticker_id :: Integer
  }
  deriving (Show, Generic, A.FromJSON)

data Attachment
  = Photo MediaDoc
  | Audio MediaDoc
  | Video MediaDoc
  | Doc MediaDoc
  | StickerA Sticker
  deriving (Show)

instance A.FromJSON Attachment where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte Attachment" $ \o -> do
      A.String media_type <- o A..: "type"
      case media_type of
        "sticker" -> StickerA <$> o A..: media_type
        "photo" -> Photo <$> o A..: media_type
        "audio" -> Audio <$> o A..: media_type
        "video" -> Video <$> o A..: media_type
        "doc" -> Doc <$> o A..: media_type
        _ -> fail "Expected Attachment"

attachmentToQuery :: Attachment -> URI.QueryParam
attachmentToQuery (StickerA s) = "sticker_id" URI.:=: show $ sticker_id s
attachmentToQuery a =
  "attachment"
    URI.:=: case a of
      Photo m -> "photo" <> mediaToQuery m
      Audio m -> "audio" <> mediaToQuery m
      Video m -> "video" <> mediaToQuery m
      Doc m -> "doc" <> mediaToQuery m
      StickerA s -> "sticker_id=" <> show (sticker_id s)
  where
    mediaToQuery :: MediaDoc -> String
    mediaToQuery MediaDoc {..} =
      show mdoc_owner_id <> "_" <> show mdoc_id
        <> maybe "" ('_' :) (T.unpack <$> mdoc_access_key)

data CallbackEvent = CallbackEvent
  { user_id :: Integer,
    peer_id :: Integer,
    event_id :: T.Text,
    payload :: A.Value,
    conversation_message_id :: Integer
  }
  deriving (Show, Generic, A.FromJSON)

data GroupEvent
  = MessageNew Message
  | MessageEvent CallbackEvent
  deriving (Show)

instance A.FromJSON GroupEvent where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.GroupEvent" $ \o -> do
      A.String eventType <- o A..: "type"
      case eventType of
        "message_new" -> MessageNew <$> o A..: "object"
        "message_event" -> MessageEvent <$> o A..: "object"
        _ -> fail "Unknown GroupEvent type"

data Keyboard = Keyboard
  { one_time :: Bool,
    buttons :: [[KeyboardButton]],
    inline :: Bool
  }
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data KeyboardButton = KeyboardButton
  { action :: KeyboardAction,
    color :: ButtonColor
  }
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data ButtonColor
  = Primary
  | Secondary
  | Negative
  | Positive
  deriving (Show, Generic)

instance A.ToJSON ButtonColor where
  toJSON = A.genericToJSON options
    where
      options = A.defaultOptions {A.constructorTagModifier = fmap toLower}

instance A.FromJSON ButtonColor where
  parseJSON =
    A.withText "FromJSON API.Vkontakte.ButtonColor" $ \case
      "primary" -> pure Primary
      "secondary" -> pure Secondary
      "negative" -> pure Negative
      "positive" -> pure Positive
      _ -> fail "Unknown color"

data KeyboardAction = KeyboardAction
  { action_type :: KeyboardActionType,
    action_label :: Maybe T.Text,
    action_payload :: Maybe A.Value,
    action_link :: Maybe T.Text
  }
  deriving (Show, Generic)

instance A.ToJSON KeyboardAction where
  toJSON = A.genericToJSON options
    where
      options = A.defaultOptions {A.fieldLabelModifier = drop 7, A.omitNothingFields = True}

instance A.FromJSON KeyboardAction where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 7}

data KeyboardActionType
  = Text
  | OpenLink
  | Location
  | Callback
  deriving (Show, Generic)

instance A.ToJSON KeyboardActionType where
  toJSON = A.genericToJSON options
    where
      options = A.defaultOptions {A.constructorTagModifier = fmap toLower}

instance A.FromJSON KeyboardActionType where
  parseJSON =
    A.withText "FromJSON API.Vkontakte.KeyboardActionType" $ \case
      "text" -> pure Text
      "openlink" -> pure OpenLink
      "location" -> pure Location
      "callback" -> pure Callback
      _ -> fail "Unknown Action Type"

data PollServer = PollServer
  { key :: T.Text,
    server :: T.Text,
    ts :: T.Text
  }
  deriving (Show, Generic, A.FromJSON)

data Error = Error
  { error_code :: Integer,
    error_msg :: T.Text
  }
  deriving (Show, Generic, A.FromJSON)

data Poll = Poll
  { ts :: T.Text,
    updates :: [GroupEvent]
  }
  deriving (Show, Generic, A.FromJSON)

data PollResponse = PollResponse Poll | PollError Error
  deriving (Show)

instance A.FromJSON PollResponse where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.PollResponse'" $ \o -> do
      asum
        [ PollError <$> o A..: "failed",
          PollResponse <$> A.parseJSON (A.Object o)
        ]

data Response
  = ErrorResponse Error
  | ResponseWith A.Value
  deriving (Show)

instance A.FromJSON Response where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.Response" $ \o -> do
      errO <- o A..:? "error" A..!= mempty
      asum
        [ ErrorResponse <$> A.parseJSON (A.Object errO),
          ResponseWith <$> o A..: "response"
        ]

data PollInitResponse
  = PollInitServer PollServer
  | PollInitError Error
  deriving (Show)

instance A.FromJSON PollInitResponse where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.PollInitResponse" $ \o -> do
      errO <- o A..:? "error" A..!= mempty
      asum
        [ PollInitError <$> A.parseJSON (A.Object errO),
          PollInitServer <$> o A..: "response"
        ]

extractUpdates :: (MonadThrow m) => PollResponse -> m [GroupEvent]
extractUpdates (PollResponse poll) = pure $ updates poll
extractUpdates (PollError c) = throwM $ apiError $ "Vkontakte Poll Error: " <> T.tshow c
