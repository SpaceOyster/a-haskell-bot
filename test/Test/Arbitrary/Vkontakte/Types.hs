{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary.Vkontakte.Types where

import API.Vkontakte.Monad as VK
import API.Vkontakte.Types as VK
import Data.Text.Extended as T
import Network.URI.Extended as URI
import Test.Arbitrary.JSON
import Test.Arbitrary.String
import Test.Arbitrary.Text
import Test.Arbitrary.URI
import Test.QuickCheck

instance Arbitrary VK.User where
  arbitrary = VK.User <$> arbitrary

instance Arbitrary VK.Message where
  arbitrary = do
    msg_id <- arbitrary
    msg_date <- arbitrary
    msg_peer_id <- arbitrary
    msg_from_id <- arbitrary
    msg_text <- getAnyText <$> arbitrary
    msg_random_id <- arbitrary
    msg_attachments <- arbitrary
    msg_payload <- fmap getJSONObject <$> arbitrary
    msg_keyboard <- arbitrary
    msg_is_cropped <- arbitrary
    pure VK.Message {..}

instance Arbitrary VK.MediaDoc where
  arbitrary = do
    mdoc_id <- arbitrary
    mdoc_owner_id <- arbitrary
    mdoc_access_key <- fmap getAnyText <$> arbitrary
    pure VK.MediaDoc {..}

instance Arbitrary VK.Sticker where
  arbitrary = VK.Sticker <$> arbitrary <*> arbitrary

instance Arbitrary VK.Attachment where
  arbitrary =
    oneof
      [ VK.Photo <$> arbitrary,
        VK.Audio <$> arbitrary,
        VK.Video <$> arbitrary,
        VK.Doc <$> arbitrary,
        VK.StickerA <$> arbitrary
      ]

instance Arbitrary VK.CallbackEvent where
  arbitrary = do
    user_id <- arbitrary
    peer_id <- arbitrary
    event_id <- getAnyText <$> arbitrary
    payload <- getJSONObject <$> arbitrary
    conversation_message_id <- arbitrary
    pure VK.CallbackEvent {..}

instance Arbitrary VK.GroupEvent where
  arbitrary = oneof [VK.MessageNew <$> arbitrary, VK.MessageEvent <$> arbitrary]

instance Arbitrary VK.KeyboardButton where
  arbitrary = VK.KeyboardButton <$> arbitrary <*> arbitrary

instance Arbitrary VK.Keyboard where
  arbitrary = VK.Keyboard <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary VK.ButtonColor where
  arbitrary =
    elements
      [ VK.Primary,
        VK.Secondary,
        VK.Negative,
        VK.Positive
      ]

instance Arbitrary VK.KeyboardActionType where
  arbitrary =
    elements
      [ VK.Text,
        VK.OpenLink,
        VK.Location,
        VK.Callback
      ]

instance Arbitrary VK.KeyboardAction where
  arbitrary = do
    action_type <- arbitrary
    action_label <- fmap getAnyText <$> arbitrary
    action_payload <- fmap getJSONObject <$> arbitrary
    action_link <- fmap getAnyText <$> arbitrary
    pure VK.KeyboardAction {..}

instance Arbitrary VK.PollServer where
  arbitrary = do
    key <- getNonEmptyCleanText <$> arbitrary
    pollURI <- getNonEmptyCleanText <$> arbitrary
    let server = T.pack "https://" <> pollURI <> T.pack "/"
    ts <- getNonEmptyCleanText <$> arbitrary
    pure VK.PollServer {..}

instance Arbitrary VK.Error where
  arbitrary = do
    error_code <- arbitrary
    error_msg <- getAnyText <$> arbitrary
    pure VK.Error {..}

instance Arbitrary VK.Poll where
  arbitrary = do
    ts <- getAnyText <$> arbitrary
    updates <- resize 10 arbitrary
    pure VK.Poll {..}

instance Arbitrary VK.PollResponse where
  arbitrary = oneof [VK.PollResponse <$> arbitrary, VK.PollError <$> arbitrary]

instance Arbitrary VK.Response where
  arbitrary =
    oneof
      [ VK.ErrorResponse <$> arbitrary,
        VK.ResponseWith . getJSONObject <$> arbitrary
      ]

instance Arbitrary VK.PollInitResponse where
  arbitrary = oneof [VK.PollInitServer <$> arbitrary, VK.PollInitError <$> arbitrary]

instance Arbitrary VK.VKState where
  arbitrary = do
    lastTS <- getAnyText <$> arbitrary
    wait <- arbitrary
    pollURI <- arbitrary
    v <- arbitrary
    key <- arbitrary
    group_id <- arbitrary
    let apiURI =
          URI.addQueryParams
            VK.vkAPIURI
            [ "v" URI.:=: v,
              "access_token" URI.:=: key,
              "group_id" URI.:=: show (group_id :: Integer)
            ]
    pure VK.VKState {..}

instance Arbitrary VK.Config where
  arbitrary = do
    key <- getNonEmptyCleanString <$> arbitrary
    group_id <- arbitrary
    v <- getNonEmptyCleanString <$> arbitrary
    wait_seconds <- arbitrary
    pure VK.Config {..}
