{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Bot.Telegram
  ( Config(..)
  , initiate
  ) where

import qualified API.Telegram as TG
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State (StateT, lift)
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))

data Config =
  Config
    { key :: String
    , defaultEchoMultiplier :: Int
    , repliesM :: Bot.RepliesM
    }
  deriving (Show)

initiate :: (MonadThrow m, Log.MonadLog m) => Config -> m TG.TGState
initiate cfg@Config {..} = do
  Log.logInfo "Initiating Telegram Bot"
  Log.logDebug $ "Telegram Bot config: " <> T.tshow cfg
  TG.initiate TG.Config {..}

instance Bot.StatefulBotMonad TG.TGState where
  type Update TG.TGState = TG.Update
  fetchUpdates ::
       (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m)
    => StateT TG.TGState m [TG.Update]
  fetchUpdates = do
    lift $ Log.logInfo "fetching Updates"
    TG.runMethod TG.GetUpdates >>= TG.extractUpdates
  data Entity TG.TGState = EMessage TG.Message
                       | ECommand TG.Message
                       | ECallback TG.CallbackQuery
                       | EOther TG.Update
  type Response TG.TGState = TG.Response
  qualifyUpdate :: TG.Update -> Bot.Entity TG.TGState
  qualifyUpdate u@TG.Update {message, callback_query}
    | Just cq <- callback_query = ECallback cq
    | Just msg <- message
    , isCommandE msg = ECommand msg
    | Just msg <- message
    , not (isCommandE msg) = EMessage msg
    | otherwise = EOther u
  reactToUpdate ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => TG.Update
    -> StateT TG.TGState m [TG.Response]
  reactToUpdate update = do
    lift $
      Log.logDebug $
      "qualifying Update with id " <> T.tshow (TG.update_id update)
    let qu = Bot.qualifyUpdate update
    case qu of
      ECommand msg -> (: []) <$> reactToCommand msg
      EMessage msg -> reactToMessage msg
      ECallback cq -> (: []) <$> reactToCallback cq
      EOther TG.Update {update_id} ->
        throwM $
        Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id
  type Message TG.TGState = TG.Message
  execCommand ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => Bot.Command
    -> (TG.Message -> StateT TG.TGState m TG.Response)
  execCommand cmd TG.Message {..} = do
    let address = TG.chat_id chat
    prompt <- lift $ Bot.repeatPrompt from
    replies <- lift BR.getReplies
    TG.runMethod $
      case cmd of
        Bot.Start -> TG.SendMessage address (Bot.greeting replies)
        Bot.Help -> TG.SendMessage address (Bot.help replies)
        Bot.Repeat -> TG.SendInlineKeyboard address prompt repeatKeyboard
        Bot.UnknownCommand -> TG.SendMessage address (Bot.unknown replies)

-- diff
reactToCommand ::
     ( MonadThrow m
     , Log.MonadLog m
     , HTTP.MonadHTTP m
     , DB.MonadUsersDB m
     , BR.MonadBotReplies m
     )
  => TG.Message
  -> StateT TG.TGState m TG.Response
reactToCommand msg@TG.Message {message_id} = do
  cmd <- getCommandThrow msg
  lift $
    Log.logDebug $
    "got command" <> T.tshow cmd <> " in message id " <> T.tshow message_id
  Bot.execCommand cmd msg

-- diff
reactToMessage ::
     (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m)
  => TG.Message
  -> StateT TG.TGState m [TG.Response]
reactToMessage msg@TG.Message {message_id} = do
  author <- TG.getAuthorThrow msg
  n <- lift $ DB.getUserMultiplier author
  lift $
    Log.logDebug $
    "generating " <> T.tshow n <> " echoes for Message: " <> T.tshow message_id
  n `replicateM` TG.runMethod (TG.CopyMessage msg)

data QueryData
  = QDRepeat Int
  | QDOther T.Text
  deriving (Show)

qualifyQuery :: T.Text -> QueryData
qualifyQuery qstring =
  case qtype of
    "repeat" -> QDRepeat $ read $ T.unpack $ T.tail qdata
    _ -> QDOther qstring
  where
    (qtype, qdata) = T.break (== '_') qstring

-- diff
reactToCallback ::
     (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m)
  => TG.CallbackQuery
  -> StateT TG.TGState m TG.Response
reactToCallback cq@TG.CallbackQuery {cq_id, from} = do
  lift $
    Log.logDebug $ "Getting query data from CallbackQuery: " <> T.tshow cq_id
  cdata <- TG.getQDataThrow cq
  let user = from
  case qualifyQuery cdata of
    QDRepeat n -> do
      lift $
        Log.logInfo $
        "Setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
      lift $ DB.setUserMultiplier user n
      TG.runMethod $ TG.AnswerCallbackQuery cq_id
    QDOther s ->
      throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

-- diff
getCommandThrow :: (MonadThrow m) => TG.Message -> m Bot.Command
getCommandThrow msg = do
  t <- TG.getTextThrow msg
  pure . Bot.parseCommand . T.takeWhile (/= ' ') . T.tail $ t

-- diff
repeatKeyboard :: TG.InlineKeyboardMarkup
repeatKeyboard = TG.InlineKeyboardMarkup [button <$> [1 .. 5]]
  where
    button :: Integer -> TG.InlineKeyboardButton
    button x =
      TG.InlineKeyboardButton
        {text = T.tshow x, callback_data = "repeat_" <> T.tshow x}

-- diff
isCommandE :: TG.Message -> Bool
isCommandE TG.Message {text} = maybe False Bot.isCommand text
