{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Bot.Telegram
    ( module Bot
    , Bot.doBotThing
    , Bot.withHandle
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

instance Bot.HandleConfig Config where
    type HandleType Config = Bot.Handle TG.APIState
    new :: Config -> Logger.Handle -> IO (Bot.Handle TG.APIState)
    new cfg@Config {..} hLog = do
        Logger.info' hLog "Initiating Telegram Bot"
        Logger.debug' hLog $ "Telegram Bot config: " <> show cfg
        state <- newIORef $ Bot.BotState {userSettings = mempty}
        hAPI <- TG.new TG.Config {..} hLog
        pure $ Bot.Handle {..}

instance Bot.BotHandle (Bot.Handle TG.APIState) where
    sendRequest :: Bot.Handle TG.APIState -> API.Request -> IO L8.ByteString
    sendRequest = API.sendRequest . Bot.hAPI
    type Update (Bot.Handle TG.APIState) = TG.Update
    fetchUpdates :: Bot.Handle TG.APIState -> IO [TG.Update]
    fetchUpdates hBot@Bot.Handle {hLog, hAPI} = do
        Logger.info' hLog "Telegram: fetching Updates"
        TG.runMethod' hAPI TG.GetUpdates >>= TG.extractUpdates
    logger :: Bot.Handle TG.APIState -> Logger.Handle
    logger = Bot.hLog
    data Entity (Bot.Handle TG.APIState) = EMessage TG.Message
                                     | ECommand TG.Message
                                     | ECallback TG.CallbackQuery
                                     | EOther TG.Update
                                         deriving (Show)
    type Response (Bot.Handle TG.APIState) = TG.Response
    qualifyUpdate :: TG.Update -> Bot.Entity (Bot.Handle TG.APIState)
    qualifyUpdate u@TG.Update {message, callback_query}
        | Just cq <- callback_query = ECallback cq
        | Just msg <- message =
            if isCommandE msg
                then ECommand msg
                else EMessage msg
        | otherwise = EOther u
    reactToUpdate :: Bot.Handle TG.APIState -> TG.Update -> IO [TG.Response]
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
    type Message (Bot.Handle TG.APIState) = TG.Message
    execCommand ::
           Bot.Handle TG.APIState
        -> Bot.Command
        -> (TG.Message -> IO TG.Response)
    execCommand hBot@Bot.Handle {..} cmd TG.Message {..} = do
        let address = (chat :: TG.Chat) & TG.chat_id
        prompt <- Bot.repeatPrompt hBot from
        TG.runMethod' hAPI $
            case cmd of
                Bot.Start -> TG.SendMessage address (Bot.greeting strings)
                Bot.Help -> TG.SendMessage address (Bot.help strings)
                Bot.Repeat ->
                    TG.SendInlineKeyboard address prompt repeatKeyboard
                Bot.UnknownCommand ->
                    TG.SendMessage address (Bot.unknown strings)

-- diff
reactToCommand :: Bot.Handle TG.APIState -> TG.Message -> IO TG.Response
reactToCommand hBot@Bot.Handle {hLog} msg@TG.Message {message_id} = do
    cmd <- getCommandThrow msg
    Logger.debug' hLog $
        "Telegram: Got command" <>
        show cmd <> " in message id " <> show message_id
    Bot.execCommand hBot cmd msg

-- diff
reactToMessage :: Bot.Handle TG.APIState -> TG.Message -> IO [TG.Response]
reactToMessage hBot@Bot.Handle {..} msg@TG.Message {message_id} = do
    author <- TG.getAuthorThrow msg
    n <- Bot.getUserMultiplier hBot author
    Logger.debug' hLog $
        "Telegram: generating " <>
        show n <> " echoes for Message: " <> show message_id
    n `replicateM` TG.runMethod' hAPI (TG.CopyMessage msg)

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
reactToCallback :: Bot.Handle TG.APIState -> TG.CallbackQuery -> IO TG.Response
reactToCallback hBot@Bot.Handle {hLog, hAPI} cq@TG.CallbackQuery {id, from} = do
    Logger.debug' hLog $ "Getting query data from CallbackQuery: " <> show id
    cdata <- TG.getQDataThrow cq
    let user = from
    case qualifyQuery cdata of
        QDRepeat n -> do
            Logger.info' hLog $
                "Setting echo multiplier = " <> show n <> " for " <> show user
            Bot.setUserMultiplier hBot user n
            TG.runMethod' hAPI $ TG.AnswerCallbackQuery id
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
