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
import Control.Monad (replicateM_)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.State (evalStateT)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB

data Config = Config
  { key :: String,
    defaultEchoMultiplier :: Int,
    repliesM :: BR.RepliesM
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
  type Message (TG.TelegramT m) = TG.Message
  type Command (TG.TelegramT m) = TG.Message
  type CallbackQuery (TG.TelegramT m) = TG.CallbackQuery

  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TG.TelegramT m [Bot.Entity (TG.TelegramT m)]
  fetchUpdates = flip catch logError $ do
    Log.logInfo "fetching Updates"
    updates <- TG.Methods.getUpdates
    pure $ [x | u <- updates, x <- toBotEntity u]
    where
      logError e = do
        Log.logError $ "Failed to fetch Updates: " <> T.tshow (e :: AppError)
        pure []

  reactToCallback ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    TG.CallbackQuery ->
    TG.TelegramT m ()
  reactToCallback cq = flip catch logError $ do
    qdata <- qualifyQuery cq
    respondToCallback qdata cq
    where
      logError e = Log.logWarning $ T.tshow (e :: AppError)

  getAuthorsSettings ::
    (DB.MonadUsersDB m) =>
    TG.Message ->
    TG.TelegramT m DB.UserData
  getAuthorsSettings msg = do
    let maybeAuthor = TG.from (msg :: TG.Message)
    maybeAuthor & DB.getUserDataM & DB.orDefaultData

  echoMessageNTimes ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TG.Message ->
    Int ->
    TG.TelegramT m ()
  echoMessageNTimes msg n = do
    Log.logDebug . mconcat $
      [ "grnerating ",
        T.tshow n,
        " echoes for Message: ",
        T.tshow (TG.message_id msg)
      ]
    replicateM_ n (TG.Methods.copyMessage msg) `catch` logError
    where
      logError e =
        Log.logError . T.unlines $
          [ "Failed to reply with echo to message: ",
            T.tshow msg,
            "\tWith Error: ",
            T.tshow (e :: AppError)
          ]

  execCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (TG.Message -> TG.TelegramT m ())
  execCommand cmd msg@TG.Message {..} = flip catch logError $ do
    Log.logDebug . mconcat $
      [ "Got command ",
        T.tshow cmd,
        " in message id ",
        T.tshow message_id
      ]
    let address = TG.chat_id chat
    prompt <- Bot.repeatPrompt from
    replies <- BR.getReplies
    () <$ case cmd of
      Bot.Start -> TG.Methods.sendMessage address (BR.greeting replies)
      Bot.Help -> TG.Methods.sendMessage address (BR.help replies)
      Bot.Repeat -> TG.Methods.sendInlineKeyboard address prompt repeatKeyboard
      Bot.UnknownCommand -> TG.Methods.sendMessage address (BR.unknown replies)
    where
      logError e =
        Log.logError . T.unlines $
          [ "Failed to execute " <> T.tshow cmd <> " from message ",
            T.tshow msg,
            "\t With Error: ",
            T.tshow (e :: AppError)
          ]

getCommand :: TG.Message -> Bot.BotCommand
getCommand TG.Message {text} =
  case text of
    Nothing -> Bot.UnknownCommand
    Just t -> Bot.parseCommand . T.takeWhile (/= ' ') . T.tail $ t

toBotEntity ::
  (MonadThrow m) =>
  TG.Update ->
  m (Bot.Entity (TG.TelegramT n))
toBotEntity u@TG.Update {message, callback_query}
  | Just cq <- callback_query = pure $ Bot.ECallback cq
  | Just msg <- message =
      if isCommandE msg
        then pure $ Bot.ECommand (getCommand msg) msg
        else pure $ Bot.EMessage msg
  | otherwise =
      throwM . botError $
        "Unknown Update Type. Update: " <> T.tshow (TG.update_id u)

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
  Log.logInfo $ "Setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
  _ <- TG.Methods.answerCallbackQuery $ TG.cq_id c
  DB.setUserMultiplier user n

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
