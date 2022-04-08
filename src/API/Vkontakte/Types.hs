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
    date :: Integer,
    peer_id :: Integer,
    from_id :: Integer,
    text :: T.Text,
    random_id :: Maybe Integer,
    attachments :: [Attachment],
    payload :: Maybe A.Value,
    keyboard :: Maybe Keyboard,
    is_cropped :: Maybe Bool
  }
  deriving (Show)

instance A.FromJSON Message where
  parseJSON =
    A.withObject "Message" $ \o -> do
      msg_id <- o A..: "id"
      date <- o A..: "date"
      peer_id <- o A..: "peer_id"
      from_id <- o A..: "from_id"
      text <- o A..: "text"
      random_id <- o A..:? "random_id"
      attachments <- o A..: "attachments"
      payload <- o A..:? "payload"
      keyboard <- o A..:? "keyboard"
      is_cropped <- o A..:? "is_cropped"
      pure $ Message {..}

data MediaDoc = MediaDoc
  { mdoc_id :: Integer,
    owner_id :: Integer,
    access_key :: Maybe T.Text
  }
  deriving (Show)

instance A.FromJSON MediaDoc where
  parseJSON =
    A.withObject "MediaDoc" $ \o -> do
      mdoc_id <- o A..: "id"
      owner_id <- o A..: "owner_id"
      access_key <- o A..: "access_key"
      pure $ MediaDoc {..}

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
      show owner_id <> "_" <> show mdoc_id
        <> maybe "" ('_' :) (T.unpack <$> access_key)

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
      options = A.defaultOptions {A.constructorTagModifier = fmap toLower} -- TODO do better

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
    label :: Maybe T.Text,
    payload :: Maybe A.Value,
    link :: Maybe T.Text
  }
  deriving (Show, Generic)

instance A.ToJSON KeyboardAction where
  toJSON KeyboardAction {..} =
    A.object $
      filter
        ((/= A.Null) . snd) -- TODO do better
        [ "type" A..= action_type,
          "label" A..= label,
          "payload" A..= payload,
          "link" A..= link
        ]

instance A.FromJSON KeyboardAction where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.KeyboardAction" $ \o -> do
      action_type <- o A..: "type"
      label <- o A..:? "label"
      payload <- o A..:? "payload"
      link <- o A..:? "link"
      pure KeyboardAction {..}

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

data Response
  = ErrorResponse Error
  | PollResponse Poll
  | PollError Integer
  | OtherResponse A.Value
  deriving (Show)

instance A.FromJSON Response where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.Response" $ \o -> do
      errO <- o A..:? "error" A..!= mempty
      asum
        [ ErrorResponse <$> A.parseJSON (A.Object errO),
          PollError <$> o A..: "failed",
          PollResponse <$> A.parseJSON (A.Object o),
          OtherResponse <$> o A..: "response"
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

extractUpdates :: (MonadThrow m) => Response -> m [GroupEvent]
extractUpdates (PollResponse poll) = pure $ updates poll
extractUpdates (PollError c) = throwM $ apiError $ "Vkontakte Poll Error: " <> T.tshow c
extractUpdates _ = throwM $ apiError "Expexted poll response"
