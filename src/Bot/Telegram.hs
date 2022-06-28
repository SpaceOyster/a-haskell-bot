{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Bot.Telegram
  ( TG.Config (..),
    TelegramBot (..),
    evalTelegramBot,
    getCommand,
    toBotEntity,
    qualifyQuery,
    respondToCallback,
    repeatKeyboard,
    isCommandE,
  )
where

import qualified API.Telegram.Methods as TG.Methods
  ( answerCallbackQuery,
    copyMessage,
    getUpdates,
    sendInlineKeyboard,
    sendMessage,
  )
import qualified API.Telegram.Monad as TG
  ( Config (..),
    MonadTelegram,
    TelegramT (..),
    evalTelegramT,
  )
import qualified API.Telegram.Types as TG
  ( CallbackQuery (..),
    Chat (..),
    InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
    Message (..),
    Update (..),
  )
import App.Error (AppError, botError)
import qualified Bot
import Control.Monad (replicateM_, void)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB

newtype TelegramBot m a = TelegramBot {unTelegramBot :: TG.TelegramT m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      TG.MonadTelegram,
      MonadTrans
    )

instance (Log.MonadLog m) => Log.MonadLog (TelegramBot m) where
  doLog priority text = lift . Log.doLog priority $ "[Telegram] " <> text

evalTelegramBot ::
  (MonadCatch m, Log.MonadLog m) =>
  TG.Config ->
  TelegramBot m a ->
  m a
evalTelegramBot cfg = TG.evalTelegramT cfg . unTelegramBot

instance
  ( MonadThrow m,
    MonadCatch m,
    Log.MonadLog m,
    HTTP.MonadHTTP m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m
  ) =>
  Bot.EchoBotMonad (TelegramBot m)
  where
  type Message (TelegramBot m) = TG.Message
  type Command (TelegramBot m) = TG.Message
  type CallbackQuery (TelegramBot m) = TG.CallbackQuery

  fetchUpdates ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TelegramBot m [Bot.Entity (TelegramBot m)]
  fetchUpdates = flip catch logError $ do
    Log.logInfo "fetching Updates"
    updates <- TG.Methods.getUpdates
    pure $ [x | u <- updates, x <- toBotEntity u]
    where
      logError e = do
        Log.logError $ "Failed to fetch Updates: " <> T.tshow (e :: AppError)
        pure []

  reactToCallback ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    TG.CallbackQuery ->
    TelegramBot m ()
  reactToCallback cq = flip catch logError $ do
    qdata <- qualifyQuery cq
    _ <- respondToCallback qdata cq
    pure ()
    where
      logError e = Log.logWarning $ T.tshow (e :: AppError)

  getAuthorsSettings ::
    (DB.MonadUsersDB m) =>
    TG.Message ->
    TelegramBot m DB.UserData
  getAuthorsSettings msg = do
    let maybeAuthor = TG.from (msg :: TG.Message)
    maybeAuthor & DB.getUserDataM & DB.orDefaultData

  echoMessageNTimes ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TG.Message ->
    Int ->
    TelegramBot m ()
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
    ( MonadCatch m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (TG.Message -> TelegramBot m ())
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
    void $ case cmd of
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
  fromMaybe Bot.UnknownCommand (text >>= parse)
  where
    parse = fmap (Bot.parseCommand . T.stripEnd) . T.stripPrefix "/"

toBotEntity :: (MonadThrow m) => TG.Update -> m (Bot.Entity (TelegramBot n))
toBotEntity u@TG.Update {message, callback_query} =
  maybe throwErr pure . asum $
    [Bot.ECallback <$> callback_query, qualify <$> message]
  where
    qualify msg =
      if isCommandE msg
        then Bot.ECommand (getCommand msg) msg
        else Bot.EMessage msg
    throwErr =
      throwM . botError $
        "Unknown Update Type. Update: " <> T.tshow (TG.update_id u)

qualifyQuery :: (MonadThrow m) => TG.CallbackQuery -> m Bot.QueryData
qualifyQuery cq = do
  let qstringM = TG.query_data cq
  case qstringM >>= Bot.parseQuery of
    Just qdata -> pure qdata
    Nothing -> throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow cq

respondToCallback ::
  (TG.MonadTelegram m, MonadThrow m, HTTP.MonadHTTP m, Log.MonadLog m, DB.MonadUsersDB m) =>
  Bot.QueryData ->
  TG.CallbackQuery ->
  m Bool
respondToCallback (Bot.QDRepeat n) c = do
  let user = TG.from (c :: TG.CallbackQuery)
  Log.logInfo $ "Setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
  DB.setUserMultiplier user n
  TG.Methods.answerCallbackQuery $ TG.cq_id c

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
