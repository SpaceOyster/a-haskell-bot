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
  deriving (Eq, Show, Generic)

instance A.FromJSON Message where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 4}

instance A.ToJSON Message where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 4}

data MediaDoc = MediaDoc
  { mdoc_id :: Integer,
    mdoc_owner_id :: Integer,
    mdoc_access_key :: Maybe T.Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON MediaDoc where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 5}

instance A.ToJSON MediaDoc where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 5}

data Sticker = Sticker
  { product_id :: Integer,
    sticker_id :: Integer
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

data Attachment
  = Photo MediaDoc
  | Audio MediaDoc
  | Video MediaDoc
  | Doc MediaDoc
  | StickerA Sticker
  deriving (Eq, Show)

instance A.FromJSON Attachment where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte Attachment" $ \o -> do
      media_type <- o A..: "type"
      case media_type of
        "sticker" -> StickerA <$> o A..: media_type
        "photo" -> Photo <$> o A..: media_type
        "audio" -> Audio <$> o A..: media_type
        "video" -> Video <$> o A..: media_type
        "doc" -> Doc <$> o A..: media_type
        _ -> fail "Expected Attachment"

instance A.ToJSON Attachment where
  toJSON att = case att of
    Photo md -> helper "photo" md
    Audio md -> helper "audio" md
    Video md -> helper "video" md
    Doc md -> helper "doc" md
    StickerA st -> helper "sticker" st
    where
      helper typ atchm = A.object ["type" A..= typ, typ A..= atchm]

attachmentToQuery :: Attachment -> URI.QueryParam
attachmentToQuery a =
  case a of
    Photo m -> "attachment" URI.:=: "photo" <> mediaToQuery m
    Audio m -> "attachment" URI.:=: "audio" <> mediaToQuery m
    Video m -> "attachment" URI.:=: "video" <> mediaToQuery m
    Doc m -> "attachment" URI.:=: "doc" <> mediaToQuery m
    StickerA s -> "sticker_id" URI.:=: show $ sticker_id s
  where
    mediaToQuery :: MediaDoc -> String
    mediaToQuery MediaDoc {..} =
      show mdoc_owner_id
        <> "_"
        <> show mdoc_id
        <> maybe "" ('_' :) (T.unpack <$> mdoc_access_key)

data CallbackEvent = CallbackEvent
  { user_id :: Integer,
    peer_id :: Integer,
    event_id :: T.Text,
    payload :: A.Value,
    conversation_message_id :: Integer
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

data GroupEvent
  = MessageNew Message
  | MessageEvent CallbackEvent
  deriving (Eq, Show)

instance A.FromJSON GroupEvent where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.GroupEvent" $ \o -> do
      A.String eventType <- o A..: "type"
      case eventType of
        "message_new" -> MessageNew <$> o A..: "object"
        "message_event" -> MessageEvent <$> o A..: "object"
        _ -> fail "Unknown GroupEvent type"

instance A.ToJSON GroupEvent where
  toJSON gEvnt = case gEvnt of
    MessageNew mes -> helper "message_new" mes
    MessageEvent ce -> helper "message_event" ce
    where
      helper :: (A.ToJSON a) => String -> a -> A.Value
      helper typ obj = A.object ["type" A..= typ, "object" A..= obj]

data Keyboard = Keyboard
  { one_time :: Bool,
    buttons :: [[KeyboardButton]],
    inline :: Bool
  }
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

data KeyboardButton = KeyboardButton
  { action :: KeyboardAction,
    color :: ButtonColor
  }
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

data ButtonColor
  = Primary
  | Secondary
  | Negative
  | Positive
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic)

instance A.ToJSON KeyboardAction where
  toJSON = A.genericToJSON options
    where
      options =
        A.defaultOptions
          { A.fieldLabelModifier = drop 7,
            A.omitNothingFields = True
          }

instance A.FromJSON KeyboardAction where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 7}

data KeyboardActionType
  = Text
  | OpenLink
  | Location
  | Callback
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

data Error = Error
  { error_code :: Integer,
    error_msg :: T.Text
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

data Poll = Poll
  { ts :: T.Text,
    updates :: [GroupEvent]
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

data PollResponse = PollResponse Poll | PollError Error
  deriving (Eq, Show)

instance A.FromJSON PollResponse where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.PollResponse'" $ \o -> do
      asum
        [ PollError <$> o A..: "failed",
          PollResponse <$> A.parseJSON (A.Object o)
        ]

instance A.ToJSON PollResponse where
  toJSON (PollResponse p) = A.toJSON p
  toJSON (PollError err) = A.object ["failed" A..= err]

data Response
  = ErrorResponse Error
  | ResponseWith A.Value
  deriving (Show)

instance A.FromJSON Response where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.Response" $ \o -> do
      asum
        [ ErrorResponse <$> o A..: "error",
          ResponseWith <$> o A..: "response"
        ]

instance A.ToJSON Response where
  toJSON (ResponseWith v) = A.object ["response" A..= v]
  toJSON (ErrorResponse v) = A.object ["error" A..= v]

data PollInitResponse
  = PollInitServer PollServer
  | PollInitError Error
  deriving (Show)

instance A.FromJSON PollInitResponse where
  parseJSON =
    A.withObject "FromJSON API.Vkontakte.PollInitResponse" $ \o -> do
      asum
        [ PollInitError <$> o A..: "error",
          PollInitServer <$> o A..: "response"
        ]

instance A.ToJSON PollInitResponse where
  toJSON (PollInitServer pollServer) = A.object ["response" A..= pollServer]
  toJSON (PollInitError err) = A.object ["error" A..= err]

extractUpdates :: (MonadThrow m) => PollResponse -> m [GroupEvent]
extractUpdates (PollResponse poll) = pure $ updates poll
extractUpdates (PollError c) =
  throwM . apiError $
    "Vkontakte Poll Error: " <> T.tshow c

fromResponse :: (A.FromJSON a, MonadThrow m) => Response -> m a
fromResponse (ErrorResponse err) =
  throwM . apiError $
    "Responded with Error: "
      <> T.tshow (error_code err)
      <> ": "
      <> error_msg (err :: Error)
fromResponse (ResponseWith v) =
  case A.fromJSON v of
    A.Success a -> pure a
    A.Error _ ->
      throwM . apiError $
        "Responded with unexpected result: " <> T.lazyDecodeUtf8 (A.encode v)
