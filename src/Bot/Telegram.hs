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
    ) where

import qualified API.Telegram as TG
import App.Monad
import qualified Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Function ((&))
import Data.IORef (newIORef)
import qualified Data.Text.Extended as T
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))
import Handle.Class (IsHandle(..))
import qualified Logger as L

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , stringsM :: Bot.StringsM
        }
    deriving (Show)

instance L.HasLog (Bot.Handle TG.Handle) where
    getLog Bot.Handle {hLog} = \p t -> L.getLog hLog p $ "Bot.Telegram: " <> t

instance IsHandle (Bot.Handle TG.Handle) Config where
    new :: Config -> L.Handle -> IO (Bot.Handle TG.Handle)
    new cfg@Config {..} hLog = do
        L.logInfo hLog "Initiating Telegram Bot"
        L.logDebug hLog $ "Telegram Bot config: " <> T.tshow cfg
        state <- newIORef $ Bot.BotState {userSettings = mempty}
        hAPI <- TG.new TG.Config {..} hLog
        let strings = Bot.fromStrinsM stringsM
        pure $ Bot.Handle {..}

instance Bot.BotHandle (Bot.Handle TG.Handle) where
    type Update (Bot.Handle TG.Handle) = TG.Update
    fetchUpdates ::
           (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
        => Bot.Handle TG.Handle
        -> m [TG.Update]
    fetchUpdates Bot.Handle {hAPI} = do
        envLogInfo "fetching Updates"
        liftIO $ TG.runMethod hAPI TG.GetUpdates >>= TG.extractUpdates
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
           (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
        => Bot.Handle TG.Handle
        -> TG.Update
        -> m [TG.Response]
    reactToUpdate hBot update = do
        envLogDebug $
            "qualifying Update with id " <> T.tshow (TG.update_id update)
        let qu = Bot.qualifyUpdate update
        case qu of
            ECommand msg -> (: []) <$> reactToCommand hBot msg
            EMessage msg -> reactToMessage hBot msg
            ECallback cq -> (: []) <$> reactToCallback hBot cq
            EOther TG.Update {update_id} ->
                throwM $
                Ex Priority.Info $
                "Unknown Update Type. Update: " ++ show update_id
    type Message (Bot.Handle TG.Handle) = TG.Message
    execCommand ::
           MonadIO m
        => Bot.Handle TG.Handle
        -> Bot.Command
        -> (TG.Message -> m TG.Response)
    execCommand hBot@Bot.Handle {..} cmd TG.Message {..} = do
        let address = (chat :: TG.Chat) & TG.chat_id
        prompt <- Bot.repeatPrompt hBot from
        liftIO . TG.runMethod hAPI $
            case cmd of
                Bot.Start -> TG.SendMessage address (Bot.greeting strings)
                Bot.Help -> TG.SendMessage address (Bot.help strings)
                Bot.Repeat ->
                    TG.SendInlineKeyboard address prompt repeatKeyboard
                Bot.UnknownCommand ->
                    TG.SendMessage address (Bot.unknown strings)

-- diff
reactToCommand ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Bot.Handle TG.Handle
    -> TG.Message
    -> m TG.Response
reactToCommand hBot msg@TG.Message {message_id} = do
    cmd <- getCommandThrow msg
    L.logDebug hBot $
        "got command" <> T.tshow cmd <> " in message id " <> T.tshow message_id
    Bot.execCommand hBot cmd msg

-- diff
reactToMessage ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Bot.Handle TG.Handle
    -> TG.Message
    -> m [TG.Response]
reactToMessage hBot@Bot.Handle {hAPI} msg@TG.Message {message_id} = do
    author <- TG.getAuthorThrow msg
    n <- Bot.getUserMultiplier hBot author
    L.logDebug hBot $
        "generating " <>
        T.tshow n <> " echoes for Message: " <> T.tshow message_id
    liftIO $ n `replicateM` TG.runMethod hAPI (TG.CopyMessage msg)

data QueryData
    = QDRepeat Int
    | QDOther String
    deriving (Show)

qualifyQuery :: String -> QueryData
qualifyQuery qstring =
    case qtype of
        "repeat" -> QDRepeat $ read (tail qdata)
        _ -> QDOther qstring
  where
    (qtype, qdata) = break (== '_') qstring

-- diff
reactToCallback ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Bot.Handle TG.Handle
    -> TG.CallbackQuery
    -> m TG.Response
reactToCallback hBot@Bot.Handle {hAPI} cq@TG.CallbackQuery {cq_id, from} = do
    L.logDebug hBot $ "Getting query data from CallbackQuery: " <> T.tshow cq_id
    cdata <- TG.getQDataThrow cq
    let user = from
    liftIO $
        case qualifyQuery cdata of
            QDRepeat n -> do
                L.logInfo hBot $
                    "Setting echo multiplier = " <>
                    T.tshow n <> " for " <> T.tshow user
                Bot.setUserMultiplier hBot user n
                TG.runMethod hAPI $ TG.AnswerCallbackQuery cq_id
            QDOther s ->
                throwM $
                Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

-- diff
getCommandThrow :: (MonadThrow m) => TG.Message -> m Bot.Command
getCommandThrow msg = do
    t <- TG.getTextThrow msg
    pure . Bot.parseCommand . takeWhile (/= ' ') . tail $ t

-- diff
repeatKeyboard :: TG.InlineKeyboardMarkup
repeatKeyboard = TG.InlineKeyboardMarkup [button <$> [1 .. 5]]
  where
    button :: Integer -> TG.InlineKeyboardButton
    button x =
        TG.InlineKeyboardButton
            {text = show x, callback_data = "repeat_" ++ show x}

-- diff
isCommandE :: TG.Message -> Bool
isCommandE TG.Message {text} = maybe False Bot.isCommand text
