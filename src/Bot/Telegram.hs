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
    Response (..),
    Response' (..),
    TGState (..),
    TelegramT (..),
    Update (..),
    getAuthorThrow,
    getQDataThrow,
    initiate,
  )
import qualified API.Telegram.Methods as TG.Methods
  ( answerCallbackQuery,
    copyMessage,
    getUpdates,
    sendInlineKeyboard,
    sendMessage,
  )
import App.Error (botError)
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow (..))
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

evalTelegramT :: (Monad m, MonadThrow m, Log.MonadLog m) => Config -> TG.TelegramT m a -> m a
evalTelegramT cfg t = do
  st <- initiate cfg
  evalStateT (TG.unTelegramT t) st

initiate :: (MonadThrow m, Log.MonadLog m) => Config -> m TG.TGState
initiate cfg@Config {..} = do
  Log.logInfo "Initiating Telegram Bot"
  Log.logDebug $ "Telegram Bot config: " <> T.tshow cfg
  TG.initiate TG.Config {..}

instance
  ( MonadThrow m,
    Log.MonadLog m,
    HTTP.MonadHTTP m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m
  ) =>
  Bot.EchoBotMonad (TG.TelegramT m)
  where
  type Update (TG.TelegramT m) = TG.Update
  type Response (TG.TelegramT m) = TG.Response'
  type Message (TG.TelegramT m) = TG.Message
  type Command (TG.TelegramT m) = TG.Message
  type CallbackQuery (TG.TelegramT m) = TG.CallbackQuery

  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    TG.TelegramT m [TG.Update]
  fetchUpdates = do
    lift $ Log.logInfo "fetching Updates"
    TG.Methods.getUpdates

  qualifyUpdate :: (MonadThrow m) => TG.Update -> TG.TelegramT m (Bot.Entity (TG.TelegramT m))
  qualifyUpdate u@TG.Update {message, callback_query}
    | Just cq <- callback_query = pure $ Bot.ECallback cq
    | Just msg <- message, isCommandE msg = pure $ Bot.ECommand msg
    | Just msg <- message, not (isCommandE msg) = pure $ Bot.EMessage msg
    | otherwise =
        throwM $
          botError $
            "Unknown Update Type. Update: " <> T.tshow (TG.update_id u)

  reactToCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    TG.Message ->
    TG.TelegramT m [TG.Response']
  reactToCommand msg@TG.Message {message_id} = do
    let cmd = getCommand msg
    lift $
      Log.logDebug $
        "got command" <> T.tshow cmd <> " in message id " <> T.tshow message_id
    pure <$> execCommand cmd msg

  reactToCallback ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
    TG.CallbackQuery ->
    TG.TelegramT m [TG.Response']
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
        pure <$> TG.Methods.answerCallbackQuery cq_id
      QDOther s ->
        throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow s

  getAuthorsSettings :: (DB.MonadUsersDB m) => TG.Message -> TG.TelegramT m DB.UserData
  getAuthorsSettings msg = do
    author <- TG.getAuthorThrow msg
    lift $ author & DB.getUserData & DB.orDefaultData

  echoMessageNTimes :: TG.Message -> Int -> TG.TelegramT m [TG.Response']
  echoMessageNTimes msg n = do
    lift $
      Log.logDebug $
        "generating " <> T.tshow n
          <> " echoes for Message: "
          <> T.tshow (TG.message_id msg)
    n `replicateM` TG.Methods.copyMessage msg

execCommand ::
  ( MonadThrow m,
    Log.MonadLog m,
    HTTP.MonadHTTP m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m
  ) =>
  Bot.BotCommand ->
  (TG.Message -> TG.TelegramT m TG.Response')
execCommand cmd TG.Message {..} = do
  let address = TG.chat_id chat
  prompt <- lift $ Bot.repeatPrompt from
  replies <- lift BR.getReplies
  case cmd of
    Bot.Start -> TG.Methods.sendMessage address (Bot.greeting replies)
    Bot.Help -> TG.Methods.sendMessage address (Bot.help replies)
    Bot.Repeat -> TG.Methods.sendInlineKeyboard address prompt repeatKeyboard
    Bot.UnknownCommand -> TG.Methods.sendMessage address (Bot.unknown replies)

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

getCommand :: TG.Message -> Bot.BotCommand
getCommand TG.Message {text} =
  case text of
    Nothing -> Bot.UnknownCommand
    Just t -> Bot.parseCommand . T.takeWhile (/= ' ') . T.tail $ t

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
