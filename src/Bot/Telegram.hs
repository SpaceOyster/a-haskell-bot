{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Telegram
    ( module Bot
    , Config(..)
    ) where

import qualified API.Class as API
import qualified API.Telegram as TG
import qualified Bot
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
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

instance {-# OVERLAPPING #-} L.HasLog (Bot.Handle TG.Handle) where
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
    fetchUpdates :: Bot.Handle TG.Handle -> IO [TG.Update]
    fetchUpdates hBot@Bot.Handle {hLog, hAPI} = do
        L.logInfo hLog "Telegram: fetching Updates"
        TG.runMethod hAPI TG.GetUpdates >>= TG.extractUpdates
    logger :: Bot.Handle TG.Handle -> L.Handle
    logger = Bot.hLog
    data Entity (Bot.Handle TG.Handle) = EMessage TG.Message
                                   | ECommand TG.Message
                                   | ECallback TG.CallbackQuery
                                   | EOther TG.Update
                                       deriving (Show)
    type Response (Bot.Handle TG.Handle) = TG.Response
    qualifyUpdate :: TG.Update -> Bot.Entity (Bot.Handle TG.Handle)
    qualifyUpdate u@TG.Update {message, callback_query}
        | Just cq <- callback_query = ECallback cq
        | Just msg <- message =
            if isCommandE msg
                then ECommand msg
                else EMessage msg
        | otherwise = EOther u
    reactToUpdate :: Bot.Handle TG.Handle -> TG.Update -> IO [TG.Response]
    reactToUpdate hBot@Bot.Handle {hLog} update = do
        L.logDebug hLog $
            "Telegram: Qualifying Update with id " <>
            T.tshow (TG.update_id update)
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
           Bot.Handle TG.Handle -> Bot.Command -> (TG.Message -> IO TG.Response)
    execCommand hBot@Bot.Handle {..} cmd TG.Message {..} = do
        let address = (chat :: TG.Chat) & TG.chat_id
        prompt <- Bot.repeatPrompt hBot from
        TG.runMethod hAPI $
            case cmd of
                Bot.Start -> TG.SendMessage address (Bot.greeting strings)
                Bot.Help -> TG.SendMessage address (Bot.help strings)
                Bot.Repeat ->
                    TG.SendInlineKeyboard address prompt repeatKeyboard
                Bot.UnknownCommand ->
                    TG.SendMessage address (Bot.unknown strings)

-- diff
reactToCommand :: Bot.Handle TG.Handle -> TG.Message -> IO TG.Response
reactToCommand hBot@Bot.Handle {hLog} msg@TG.Message {message_id} = do
    cmd <- getCommandThrow msg
    L.logDebug hLog $
        "Telegram: Got command" <>
        T.tshow cmd <> " in message id " <> T.tshow message_id
    Bot.execCommand hBot cmd msg

-- diff
reactToMessage :: Bot.Handle TG.Handle -> TG.Message -> IO [TG.Response]
reactToMessage hBot@Bot.Handle {..} msg@TG.Message {message_id} = do
    author <- TG.getAuthorThrow msg
    n <- Bot.getUserMultiplier hBot author
    L.logDebug hLog $
        "Telegram: generating " <>
        T.tshow n <> " echoes for Message: " <> T.tshow message_id
    n `replicateM` TG.runMethod hAPI (TG.CopyMessage msg)

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
reactToCallback :: Bot.Handle TG.Handle -> TG.CallbackQuery -> IO TG.Response
reactToCallback hBot@Bot.Handle {hLog, hAPI} cq@TG.CallbackQuery {id, from} = do
    L.logDebug hLog $ "Getting query data from CallbackQuery: " <> T.tshow id
    cdata <- TG.getQDataThrow cq
    let user = from
    case qualifyQuery cdata of
        QDRepeat n -> do
            L.logInfo hLog $
                "Setting echo multiplier = " <>
                T.tshow n <> " for " <> T.tshow user
            Bot.setUserMultiplier hBot user n
            TG.runMethod hAPI $ TG.AnswerCallbackQuery id
        QDOther s ->
            throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

-- diff
getCommandThrow :: (MonadThrow m) => TG.Message -> m Bot.Command
getCommandThrow msg = do
    t <- TG.getTextThrow msg
    pure . Bot.parseCommand . takeWhile (/= ' ') . tail $ t

-- diff
repeatKeyboard :: TG.InlineKeyboardMarkup
repeatKeyboard = TG.InlineKeyboardMarkup [button <$> [1 .. 5]]
  where
    button x =
        TG.InlineKeyboardButton
            {text = show x, callback_data = "repeat_" ++ show x}

-- diff
isCommandE :: TG.Message -> Bool
isCommandE TG.Message {text} = maybe False Bot.isCommand text
