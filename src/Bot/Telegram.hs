{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Telegram
  ( Config(..)
  , new
  ) where

import qualified API.Telegram as TG
import qualified Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT, get, lift, modify', put)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))

data Config =
  Config
    { key :: String
    , defaultEchoMultiplier :: Int
    , stringsM :: Bot.StringsM
    }
  deriving (Show)

new ::
     (MonadIO m, MonadThrow m, Log.MonadLog m)
  => Config
  -> StateT TG.TGState m (Bot.Handle TG.Handle)
new cfg@Config {..} = do
  lift $ Log.logInfo "Initiating Telegram Bot"
  lift $ Log.logDebug $ "Telegram Bot config: " <> T.tshow cfg
  hAPI <- TG.new TG.Config {..}
  let strings = Bot.fromStrinsM stringsM
  pure $ Bot.Handle {..}

instance Bot.BotHandle (Bot.Handle TG.Handle) where
  type APIState (Bot.Handle TG.Handle) = TG.TGState
  type Update (Bot.Handle TG.Handle) = TG.Update
  fetchUpdates ::
       (MonadIO m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m)
    => Bot.Handle TG.Handle
    -> StateT TG.TGState m [TG.Update]
  fetchUpdates _ = do
    lift $ Log.logInfo "fetching Updates"
    TG.runMethod TG.GetUpdates >>= TG.extractUpdates
  data Entity (Bot.Handle TG.Handle) = EMessage TG.Message
                                   | ECommand TG.Message
                                   | ECallback TG.CallbackQuery
                                   | EOther TG.Update
  type Response (Bot.Handle TG.Handle) = TG.Response
  qualifyUpdate :: TG.Update -> Bot.Entity (Bot.Handle TG.Handle)
  qualifyUpdate u@TG.Update {message, callback_query}
    | Just cq <- callback_query = ECallback cq
    | Just msg <- message
    , isCommandE msg = ECommand msg
    | Just msg <- message
    , not (isCommandE msg) = EMessage msg
    | otherwise = EOther u
  reactToUpdate ::
       ( MonadIO m
       , MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       )
    => Bot.Handle TG.Handle
    -> TG.Update
    -> StateT TG.TGState m [TG.Response]
  reactToUpdate hBot update = do
    lift $
      Log.logDebug $
      "qualifying Update with id " <> T.tshow (TG.update_id update)
    let qu = Bot.qualifyUpdate update
    case qu of
      ECommand msg -> (: []) <$> reactToCommand hBot msg
      EMessage msg -> reactToMessage hBot msg
      ECallback cq -> (: []) <$> reactToCallback hBot cq
      EOther TG.Update {update_id} ->
        throwM $
        Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id
  type Message (Bot.Handle TG.Handle) = TG.Message
  execCommand ::
       ( MonadIO m
       , MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       )
    => Bot.Handle TG.Handle
    -> Bot.Command
    -> (TG.Message -> StateT TG.TGState m TG.Response)
  execCommand hBot@Bot.Handle {..} cmd TG.Message {..} = do
    let address = chat & TG.chat_id
    prompt <- lift $ Bot.repeatPrompt hBot from
    TG.runMethod $
      case cmd of
        Bot.Start -> TG.SendMessage address (Bot.greeting strings)
        Bot.Help -> TG.SendMessage address (Bot.help strings)
        Bot.Repeat -> TG.SendInlineKeyboard address prompt repeatKeyboard
        Bot.UnknownCommand -> TG.SendMessage address (Bot.unknown strings)

-- diff
reactToCommand ::
     ( MonadIO m
     , MonadThrow m
     , Log.MonadLog m
     , HTTP.MonadHTTP m
     , DB.MonadUsersDB m
     )
  => Bot.Handle TG.Handle
  -> TG.Message
  -> StateT TG.TGState m TG.Response
reactToCommand hBot msg@TG.Message {message_id} = do
  cmd <- getCommandThrow msg
  lift $
    Log.logDebug $
    "got command" <> T.tshow cmd <> " in message id " <> T.tshow message_id
  Bot.execCommand hBot cmd msg

-- diff
reactToMessage ::
     ( MonadIO m
     , MonadThrow m
     , Log.MonadLog m
     , HTTP.MonadHTTP m
     , DB.MonadUsersDB m
     )
  => Bot.Handle TG.Handle
  -> TG.Message
  -> StateT TG.TGState m [TG.Response]
reactToMessage _ msg@TG.Message {message_id} = do
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
     ( MonadIO m
     , MonadThrow m
     , Log.MonadLog m
     , HTTP.MonadHTTP m
     , DB.MonadUsersDB m
     )
  => Bot.Handle TG.Handle
  -> TG.CallbackQuery
  -> StateT TG.TGState m TG.Response
reactToCallback _ cq@TG.CallbackQuery {cq_id, from} = do
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
