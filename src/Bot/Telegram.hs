{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    TGState (..),
    TelegramT (..),
    Update (..),
    initiate,
  )
import qualified API.Telegram.Methods as TG.Methods
  ( answerCallbackQuery,
    copyMessage,
    getUpdates,
    sendInlineKeyboard,
    sendMessage,
  )
import App.Error (AppError, botError)
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM_)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.State (evalStateT, lift)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB

data Config = Config
  { key :: String,
    defaultEchoMultiplier :: Int,
    repliesM :: Bot.RepliesM
  }
  deriving (Show)

evalTelegramT ::
  (Monad m, MonadCatch m, Log.MonadLog m) =>
  Config ->
  TG.TelegramT m a ->
  m a
evalTelegramT cfg t = do
  st <- initiate cfg
  evalStateT (TG.unTelegramT t) st

initiate :: (MonadCatch m, Log.MonadLog m) => Config -> m TG.TGState
initiate cfg@Config {..} = do
  Log.logInfo "Initiating Telegram Bot"
  Log.logDebug $ "Telegram Bot config: " <> T.tshow cfg
  TG.initiate TG.Config {..} `catch` \e -> do
    Log.logError "Failed to initiate Telegram Poll API"
    throwM (e :: AppError)

instance
  ( MonadThrow m,
    MonadCatch m,
    Log.MonadLog m,
    HTTP.MonadHTTP m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m
  ) =>
  Bot.EchoBotMonad (TG.TelegramT m)
  where
  type Update (TG.TelegramT m) = TG.Update
  type Message (TG.TelegramT m) = TG.Message
  type Command (TG.TelegramT m) = TG.Message
  type CallbackQuery (TG.TelegramT m) = TG.CallbackQuery

  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TG.TelegramT m [TG.Update]
  fetchUpdates = do
    lift $ Log.logInfo "fetching Updates"
    TG.Methods.getUpdates `catch` \e -> do
      lift . Log.logError $ "Failed to fetch Updates: " <> T.tshow (e :: AppError)
      pure []

  qualifyUpdate :: (MonadThrow m) => TG.Update -> TG.TelegramT m (Bot.Entity (TG.TelegramT m))
  qualifyUpdate u@TG.Update {message, callback_query}
    | Just cq <- callback_query = pure $ Bot.ECallback cq
    | Just msg <- message, isCommandE msg = pure $ Bot.ECommand msg
    | Just msg <- message, not (isCommandE msg) = pure $ Bot.EMessage msg
    | otherwise =
        throwM . botError $
          "Unknown Update Type. Update: " <> T.tshow (TG.update_id u)

  reactToCallback ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    TG.CallbackQuery ->
    TG.TelegramT m ()
  reactToCallback cq =
    case qualifyQuery cq of
      Just qdata -> respondToCallback qdata cq
      Nothing -> throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow cq

  getAuthorsSettings :: (DB.MonadUsersDB m) => TG.Message -> TG.TelegramT m DB.UserData
  getAuthorsSettings msg = do
    let maybeAuthor = TG.from (msg :: TG.Message)
    lift $ maybeAuthor & DB.getUserDataM & DB.orDefaultData

  echoMessageNTimes :: TG.Message -> Int -> TG.TelegramT m ()
  echoMessageNTimes msg n = do
    lift $
      Log.logDebug $
        "generating " <> T.tshow n
          <> " echoes for Message: "
          <> T.tshow (TG.message_id msg)
    n `replicateM_` TG.Methods.copyMessage msg

  getCommand :: (Monad m) => TG.Message -> TG.TelegramT m Bot.BotCommand
  getCommand TG.Message {text} =
    pure $ case text of
      Nothing -> Bot.UnknownCommand
      Just t -> Bot.parseCommand . T.takeWhile (/= ' ') . T.tail $ t

  execCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (TG.Message -> TG.TelegramT m ())
  execCommand cmd TG.Message {..} = do
    lift $
      Log.logDebug $
        "got command " <> T.tshow cmd <> " in message id " <> T.tshow message_id
    let address = TG.chat_id chat
    prompt <- lift $ Bot.repeatPrompt from
    replies <- lift BR.getReplies
    _ <- case cmd of
      Bot.Start -> TG.Methods.sendMessage address (Bot.greeting replies)
      Bot.Help -> TG.Methods.sendMessage address (Bot.help replies)
      Bot.Repeat -> TG.Methods.sendInlineKeyboard address prompt repeatKeyboard
      Bot.UnknownCommand -> TG.Methods.sendMessage address (Bot.unknown replies)
    pure ()

qualifyQuery :: (MonadThrow m) => TG.CallbackQuery -> m Bot.QueryData
qualifyQuery cq = do
  let qstringM = TG.query_data cq
  case qstringM >>= Bot.parseQuery of
    Just qdata -> pure qdata
    Nothing -> throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow cq

respondToCallback ::
  (MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m, DB.MonadUsersDB m) =>
  Bot.QueryData ->
  TG.CallbackQuery ->
  TG.TelegramT m ()
respondToCallback (Bot.QDRepeat n) c = do
  let user = TG.from (c :: TG.CallbackQuery)
  lift $ Log.logInfo $ "Setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
  _ <- TG.Methods.answerCallbackQuery $ TG.cq_id c
  lift $ DB.setUserMultiplier user n

repeatKeyboard :: TG.InlineKeyboardMarkup
repeatKeyboard = TG.InlineKeyboardMarkup [button <$> [1 .. 5]]
  where
    button :: Int -> TG.InlineKeyboardButton
    button x =
      TG.InlineKeyboardButton
        { text = T.tshow x,
          callback_data = Bot.encodeQuery $ Bot.QDRepeat x
        }

isCommandE :: TG.Message -> Bool
isCommandE TG.Message {text} = maybe False Bot.isCommand text
