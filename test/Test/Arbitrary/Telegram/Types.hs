{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary.Telegram.Types where

import API.Telegram.Monad (Config (..), TGState (..), tgAPIURI)
import API.Telegram.Types as TG
  ( CallbackQuery (..),
    Chat (Chat),
    Error (..),
    InlineKeyboardButton (..),
    Message (..),
    Update (..),
    User (User),
  )
import Network.URI (URI (uriPath))
import Test.Arbitrary.String (CleanString (getCleanString))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Arbitrary.Text (AnyText (getAnyText), ShortCleanText (getShortCleanText))

instance Arbitrary TG.Message where
  arbitrary = do
    message_id <- arbitrary
    from <- arbitrary
    chat <- arbitrary
    date <- arbitrary
    text <- fmap getAnyText <$> arbitrary
    pure TG.Message {..}

instance Arbitrary TG.CallbackQuery where
  arbitrary = do
    cq_id <- getAnyText <$> arbitrary
    from <- arbitrary
    message <- arbitrary
    inline_message_id <- fmap getAnyText <$> arbitrary
    query_data <- fmap getAnyText <$> arbitrary
    pure TG.CallbackQuery {..}

instance Arbitrary TG.User where
  arbitrary = User <$> arbitrary

instance Arbitrary TG.Chat where
  arbitrary = Chat <$> arbitrary

instance Arbitrary TG.Update where
  arbitrary = do
    update_id <- arbitrary
    message <- arbitrary
    callback_query <- arbitrary
    pure TG.Update {..}

instance Arbitrary InlineKeyboardButton where
  arbitrary = do
    text <- getShortCleanText <$> arbitrary
    callback_data <- getShortCleanText <$> arbitrary
    pure TG.InlineKeyboardButton {..}

instance Arbitrary TG.Error where
  arbitrary = do
    error_code <- arbitrary
    description <- getAnyText <$> arbitrary
    pure TG.Error {..}

instance Arbitrary Config where
  arbitrary = do
    key <- getCleanString <$> arbitrary
    timeout_seconds <- arbitrary
    pure Config {..}

instance Arbitrary TGState where
  arbitrary = do
    lastUpdate <- arbitrary
    key <- arbitrary
    let apiURI = tgAPIURI {uriPath = uriPath tgAPIURI <> key <> "/"}
    timeout <- arbitrary
    pure TGState {..}
