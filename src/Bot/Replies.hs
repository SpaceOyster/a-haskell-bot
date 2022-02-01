{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Replies
  ( RepliesM(..)
  , Replies(..)
  , fromRepliesM
  ) where

import Control.Applicative ((<|>))
import Data.Has
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import Prelude hiding (repeat)

data RepliesM =
  RepliesM
    { helpM :: Maybe T.Text
    , greetingM :: Maybe T.Text
    , repeatM :: Maybe T.Text
    , unknownM :: Maybe T.Text
    , settingsSavedM :: Maybe T.Text
    }
  deriving (Show)

data Replies =
  Replies
    { help :: T.Text
    , greeting :: T.Text
    , repeat :: T.Text
    , unknown :: T.Text
    , settingsSaved :: T.Text
    }
  deriving (Show)

instance Has Replies Replies where
  obtain = id

fromRepliesM :: RepliesM -> Replies
fromRepliesM RepliesM {..} =
  Replies
    { help = fromMaybe "" helpM
    , greeting = fromMaybe "" greetingM
    , repeat = fromMaybe "" repeatM
    , unknown = fromMaybe "" unknownM
    , settingsSaved = fromMaybe "" settingsSavedM
    }

instance Semigroup RepliesM where
  s0 <> s1 =
    RepliesM
      { helpM = helpM s0 <|> helpM s1
      , greetingM = greetingM s0 <|> greetingM s1
      , repeatM = repeatM s0 <|> repeatM s1
      , unknownM = unknownM s0 <|> unknownM s1
      , settingsSavedM = settingsSavedM s0 <|> settingsSavedM s1
      }

instance Monoid RepliesM where
  mempty =
    RepliesM
      { helpM = mempty
      , greetingM = mempty
      , repeatM = mempty
      , unknownM = mempty
      , settingsSavedM = mempty
      }
