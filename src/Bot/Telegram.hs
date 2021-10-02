{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Bot.Telegram
    ( module Bot
    , doBotThing
    , withHandle
    , Config(..)
    ) where

import qualified API
import qualified API.Telegram as TG
import qualified API.Telegram.Types as TG
import qualified Bot
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (newIORef)
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))
import qualified Logger
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        }
    deriving (Show)

instance Bot.BotHandle (Bot.Handle TG.APIState) where
    type Update (Bot.Handle TG.APIState) = TG.Update
    logger = Bot.hLog
    data Entity (Bot.Handle TG.APIState) = EMessage TG.Message
                                     | ECommand TG.Message
                                     | ECallback TG.CallbackQuery
                                     | EOther TG.Update
                                         deriving (Show)
    reactToUpdate :: Bot.Handle TG.APIState -> TG.Update -> IO [API.Request]
    reactToUpdate hBot@Bot.Handle {hLog} update = do
        Logger.debug' hLog $
            "Telegram: Qualifying Update with id " <> show (TG.update_id update)
        let qu = Bot.qualifyUpdate update
        case qu of
            ECommand msg -> (: []) <$> reactToCommand hBot msg
            EMessage msg -> reactToMessage hBot msg
            ECallback cq -> (: []) <$> reactToCallback hBot cq
            EOther TG.Update {update_id} ->
                throwM $
                Ex Priority.Info $
                "Unknown Update Type. Update: " ++ show update_id

-- diff
new :: Config -> Logger.Handle -> IO (Bot.Handle TG.APIState)
new cfg@Config {..} hLog = do
    Logger.info' hLog "Initiating Telegram Bot"
    Logger.debug' hLog $ "Telegram Bot config: " <> show cfg
    state <- newIORef $ Bot.BotState {userSettings = mempty}
    hAPI <- TG.new TG.Config {..} hLog
    pure $ Bot.Handle {..}

withHandle ::
       Config -> Logger.Handle -> (Bot.Handle TG.APIState -> IO a) -> IO a
withHandle config hLog io = do
    hBot <- new config hLog
    io hBot

doBotThing :: Bot.Handle TG.APIState -> IO [L8.ByteString]
doBotThing hBot@Bot.Handle {hLog} = do
    updates <- fetchUpdates hBot
    requests <- reactToUpdates hBot updates
    Logger.info' hLog $
        "Telegram: sending " <> show (length requests) <> " responses"
    mapM (hBot & Bot.hAPI & API.sendRequest) requests

fetchUpdates :: Bot.Handle TG.APIState -> IO [TG.Update]
fetchUpdates hBot@Bot.Handle {hLog, hAPI} = do
    Logger.info' hLog "Telegram: fetching Updates"
    req <- hAPI & TG.getUpdates
    json <- hAPI & API.sendRequest $ req
    Logger.debug' hLog "Telegram: decoding json response"
    resp <- throwDecode json
    TG.extractUpdates =<< TG.rememberLastUpdate hAPI resp

reactToUpdates :: Bot.Handle TG.APIState -> [TG.Update] -> IO [API.Request]
reactToUpdates hBot@Bot.Handle {hLog} updates = do
    Logger.info' hLog "Telegram: processing each update"
    join <$> mapM (reactToUpdate hBot) updates

data Entity
    = EMessage TG.Message
    | ECommand TG.Message
    | ECallback TG.CallbackQuery
    | EOther TG.Update
    deriving (Show)

-- diff
qualifyUpdate :: TG.Update -> Entity
qualifyUpdate u@TG.Update {message, callback_query}
    | Just cq <- callback_query = ECallback cq
    | Just msg <- message =
        if isCommandE msg
            then ECommand msg
            else EMessage msg
    | otherwise = EOther u

-- diff
reactToUpdate :: Bot.Handle TG.APIState -> TG.Update -> IO [API.Request]
reactToUpdate hBot@Bot.Handle {hLog} update = do
    Logger.debug' hLog $
        "Telegram: Qualifying Update with id " <> show (TG.update_id update)
    let qu = qualifyUpdate update
    case qu of
        ECommand msg -> (: []) <$> reactToCommand hBot msg
        EMessage msg -> reactToMessage hBot msg
        ECallback cq -> (: []) <$> reactToCallback hBot cq
        EOther TG.Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id

-- diff
reactToCommand :: Bot.Handle TG.APIState -> TG.Message -> IO API.Request
reactToCommand hBot@Bot.Handle {hLog} msg@TG.Message {message_id} = do
    cmd <- getCommandThrow msg
    Logger.debug' hLog $
        "Telegram: Got command" <>
        show cmd <> " in message id " <> show message_id
    let action = commandAction cmd
    runAction action hBot msg

-- diff
reactToMessage :: Bot.Handle TG.APIState -> TG.Message -> IO [API.Request]
reactToMessage hBot@Bot.Handle {..} msg@TG.Message {message_id} = do
    author <- TG.getAuthorThrow msg
    n <- Bot.getUserMultiplier hBot author
    Logger.debug' hLog $
        "Telegram: generating " <>
        show n <> " echoes for Message: " <> show message_id
    n `replicateM` TG.copyMessage hAPI msg

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
reactToCallback :: Bot.Handle TG.APIState -> TG.CallbackQuery -> IO API.Request
reactToCallback hBot@Bot.Handle {hLog} cq@TG.CallbackQuery {id, from} = do
    Logger.debug' hLog $ "Getting query data from CallbackQuery: " <> show id
    cdata <- TG.getQDataThrow cq
    let user = from
    case qualifyQuery cdata of
        QDRepeat n -> do
            Logger.info' hLog $
                "Setting echo multiplier = " <> show n <> " for " <> show user
            Bot.setUserMultiplier hBot user n
            TG.answerCallbackQuery (Bot.hAPI hBot) id
        QDOther s ->
            throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

-- diff
commandAction :: Bot.Command -> Action TG.APIState IO
commandAction cmd =
    Action $ \hBot@Bot.Handle {..} TG.Message {..} -> do
        let address = (chat :: TG.Chat) & TG.chat_id
        case cmd of
            Bot.Start -> TG.sendMessage hAPI address $ Bot.greeting strings
            Bot.Help -> TG.sendMessage hAPI address $ Bot.help strings
            Bot.Repeat -> do
                prompt <- Bot.repeatPrompt hBot from
                TG.sendInlineKeyboard hAPI address prompt repeatKeyboard
            Bot.UnknownCommand ->
                TG.sendMessage hAPI address $ Bot.unknown strings

-- diff
getCommandThrow :: (MonadThrow m) => TG.Message -> m Bot.Command
getCommandThrow msg = do
    t <- TG.getTextThrow msg
    pure . Bot.parseCommand . takeWhile (/= ' ') . tail $ t

newtype Action s m =
    Action
        { runAction :: Bot.Handle s -> TG.Message -> m API.Request
        }

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
