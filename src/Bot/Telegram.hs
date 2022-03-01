{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bot.Telegram
  ( Config (..),
    initiate,
    evalTelegramT,
    TG.TelegramT (..),
  )
where

import qualified API.Telegram as TG
  ( CallbackQuery (..),
    Chat (..),
    Config (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    Message (..),
    Method (..),
    Response (..),
    TGState (..),
    TelegramT (..),
    Update (..),
    extractUpdates,
    getAuthorThrow,
    getQDataThrow,
    getTextThrow,
    initiate,
    runMethod,
  )
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (MonadState, StateT, evalStateT, lift)
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import Exceptions (BotException (..))
import qualified Exceptions as Priority (Priority (..))

data Config = Config
  { key :: String,
    defaultEchoMultiplier :: Int,
    repliesM :: Bot.RepliesM
  }
  deriving (Show)

evalTelegramT :: (Monad m, MonadThrow m, Log.MonadLog m) => Config -> TG.TelegramT m a -> m a
evalTelegramT cfg t = do
  st <- initiate cfg
  evalStateT (TG.unTelegramT t) st

initiate :: (MonadThrow m, Log.MonadLog m) => Config -> m TG.TGState
initiate cfg@Config {..} = do
  Log.logInfo "Initiating Telegram Bot"
  Log.logDebug $ "Telegram Bot config: " <> T.tshow cfg
  TG.initiate TG.Config {..}

instance Bot.StatefulBotMonad TG.TelegramT where
  type Update TG.TelegramT = TG.Update
  type Response TG.TelegramT = TG.Response
  type Message TG.TelegramT = TG.Message
  type Command TG.TelegramT = TG.Message
  type CallbackQuery TG.TelegramT = TG.CallbackQuery

  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TG.TelegramT m [TG.Update]
  fetchUpdates = do
    lift $ Log.logInfo "fetching Updates"
    TG.runMethod TG.GetUpdates >>= TG.extractUpdates

  qualifyUpdate :: (MonadThrow m) => TG.Update -> m (Bot.Entity TG.TelegramT)
  qualifyUpdate u@TG.Update {message, callback_query}
    | Just cq <- callback_query = pure $ Bot.ECallback cq
    | Just msg <- message,
      isCommandE msg =
        pure $ Bot.ECommand msg
    | Just msg <- message,
      not (isCommandE msg) =
        pure $ Bot.EMessage msg
    | otherwise =
        throwM $
          Ex Priority.Info $
            "Unknown Update Type. Update: " ++ show (TG.update_id u)
  reactToUpdate ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    TG.Update ->
    TG.TelegramT m [TG.Response]
  reactToUpdate update = do
    lift $
      Log.logDebug $
        "qualifying Update with id " <> T.tshow (TG.update_id update)
    qu <- Bot.qualifyUpdate @TG.TelegramT update
    case qu of
      Bot.ECommand msg -> Bot.reactToCommand msg
      Bot.EMessage msg -> Bot.reactToMessage msg
      Bot.ECallback cq -> Bot.reactToCallback cq
  execCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (TG.Message -> TG.TelegramT m TG.Response)
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

  reactToCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    TG.Message ->
    TG.TelegramT m [TG.Response]
  reactToCommand msg@TG.Message {message_id} = do
    cmd <- getCommandThrow msg
    lift $
      Log.logDebug $
        "got command" <> T.tshow cmd <> " in message id " <> T.tshow message_id
    pure <$> Bot.execCommand cmd msg

  reactToMessage ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    TG.Message ->
    TG.TelegramT m [TG.Response]
  reactToMessage msg@TG.Message {message_id} = do
    author <- TG.getAuthorThrow msg
    n <- lift $ DB.getUserMultiplier author
    lift $
      Log.logDebug $
        "generating " <> T.tshow n <> " echoes for Message: " <> T.tshow message_id
    n `replicateM` TG.runMethod (TG.CopyMessage msg)

  reactToCallback ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    TG.CallbackQuery ->
    TG.TelegramT m [TG.Response]
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
        pure <$> (TG.runMethod $ TG.AnswerCallbackQuery cq_id)
      QDOther s ->
        throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

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

getCommandThrow :: (MonadThrow m) => TG.Message -> m Bot.BotCommand
getCommandThrow msg = do
  t <- TG.getTextThrow msg
  pure . Bot.parseCommand . T.takeWhile (/= ' ') . T.tail $ t

repeatKeyboard :: TG.InlineKeyboardMarkup
repeatKeyboard = TG.InlineKeyboardMarkup [button <$> [1 .. 5]]
  where
    button :: Integer -> TG.InlineKeyboardButton
    button x =
      TG.InlineKeyboardButton
        { text = T.tshow x,
          callback_data = "repeat_" <> T.tshow x
        }

isCommandE :: TG.Message -> Bool
isCommandE TG.Message {text} = maybe False Bot.isCommand text
