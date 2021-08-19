{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Telegram
    ( module Bot
    , doBotThing
    , withHandle
    , Config(..)
    ) where

import qualified API
import qualified API.Telegram as TG
import API.Telegram.Types
import Bot hiding (strings)
import qualified Bot (strings)
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (newIORef)
import Data.List.Extended (replaceSubseq)
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

new :: Config -> Logger.Handle -> IO (Handle TG.APIState)
new cfg@Config {..} hLog = do
    Logger.info' hLog "Initiating Telegram Bot"
    Logger.debug' hLog $ "Telegram Bot config: " <> show cfg
    state <- newIORef $ BotState {userSettings = mempty}
    hAPI <- TG.new TG.Config {..} hLog
    pure $ Handle {..}

withHandle :: Config -> Logger.Handle -> (Handle TG.APIState -> IO a) -> IO a
withHandle config hLog io = do
    hBot <- new config hLog
    io hBot

doBotThing :: Handle TG.APIState -> IO [L8.ByteString]
doBotThing hBot@Handle {hLog} = do
    updates <- fetchUpdates hBot
    requests <- reactToUpdates hBot updates
    Logger.info' hLog $
        "Telegram: sending " <> show (length requests) <> " responses"
    mapM (hBot & hAPI & API.sendRequest) requests

fetchUpdates :: Handle TG.APIState -> IO [Update]
fetchUpdates hBot@Handle {hLog, hAPI} = do
    Logger.info' hLog "Telegram: fetching Updates"
    req <- hAPI & TG.getUpdates
    json <- hAPI & API.sendRequest $ req
    Logger.debug' hLog "Telegram: decoding json response"
    resp <- throwDecode json
    extractUpdates =<< TG.rememberLastUpdate hAPI resp

reactToUpdates :: Handle TG.APIState -> [Update] -> IO [API.Request]
reactToUpdates hBot@Handle {hLog} updates = do
    Logger.info' hLog "Telegram: processing each update"
    join <$> mapM (reactToUpdate hBot) updates

data Entity
    = EMessage Message
    | ECommand Message
    | ECallback CallbackQuery
    | EOther Update
    deriving (Show)

qualifyUpdate :: Update -> Entity
qualifyUpdate u@Update {message, callback_query}
    | Just cq <- callback_query = ECallback cq
    | Just msg <- message =
        if isCommandE msg
            then ECommand msg
            else EMessage msg
    | otherwise = EOther u

reactToUpdate :: Handle TG.APIState -> Update -> IO [API.Request]
reactToUpdate hBot@Handle {hLog} update = do
    Logger.debug' hLog $
        "Telegram: Qualifying Update with id " <> show (update_id update)
    let qu = qualifyUpdate update
    case qu of
        ECommand msg -> (: []) <$> reactToCommand hBot msg
        EMessage msg -> reactToMessage hBot msg
        ECallback cq -> (: []) <$> reactToCallback hBot cq
        EOther Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id

reactToCommand :: Handle TG.APIState -> Message -> IO API.Request
reactToCommand hBot@Handle {hLog} msg@Message {message_id} = do
    cmd <- getCommandThrow msg
    Logger.debug' hLog $
        "Telegram: Got command" <>
        show cmd <> " in message id " <> show message_id
    let action = commandAction cmd
    runAction action hBot msg

reactToMessage :: Handle TG.APIState -> Message -> IO [API.Request]
reactToMessage hBot@Handle {..} msg@Message {message_id} = do
    author <- getAuthorThrow msg
    n <- hBot `getUserMultiplier` author
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

reactToCallback :: Handle TG.APIState -> CallbackQuery -> IO API.Request
reactToCallback hBot@Handle {hLog} cq@CallbackQuery {id, from} = do
    Logger.debug' hLog $ "Getting query data from CallbackQuery: " <> show id
    cdata <- getQDataThrow cq
    let user = from
    case qualifyQuery cdata of
        QDRepeat n -> do
            Logger.info' hLog $
                "Setting echo multiplier = " <> show n <> " for " <> show user
            setUserMultiplier hBot user n
            TG.answerCallbackQuery (hAPI hBot) id
        QDOther s ->
            throwM $ Ex Priority.Info $ "Unknown CallbackQuery type: " ++ show s

commandAction :: Command -> Action TG.APIState IO
commandAction cmd =
    Action $ \hBot@Handle {..} Message {..} -> do
        let address = (chat :: Chat) & chat_id
        case cmd of
            Start -> TG.sendMessage hAPI address $ Bot.greeting strings
            Help -> TG.sendMessage hAPI address $ Bot.help strings
            Repeat -> do
                prompt <- hBot & repeatPrompt $ from
                TG.sendInlineKeyboard hAPI address prompt repeatKeyboard
            UnknownCommand -> TG.sendMessage hAPI address $ Bot.unknown strings

getCommandThrow :: (MonadThrow m) => Message -> m Command
getCommandThrow msg = do
    t <- getTextThrow msg
    pure . parseCommand . takeWhile (/= ' ') . tail $ t

newtype Action s m =
    Action
        { runAction :: Handle s -> Message -> m API.Request
        }

repeatPrompt :: Handle s -> Maybe User -> IO String
repeatPrompt hBot userM = do
    mult <- hBot & getUserMultiplierM $ userM
    let prompt' = hBot & Bot.strings & Bot.repeat
    pure $ replaceSubseq prompt' "%n" (show mult)

repeatKeyboard :: InlineKeyboardMarkup
repeatKeyboard =
    InlineKeyboardMarkup [[button 1, button 2, button 3, button 4, button 5]]
  where
    button x =
        InlineKeyboardButton
            {text = show x, callback_data = "repeat_" ++ show x}

isCommandE :: Message -> Bool
isCommandE Message {text} = maybe False isCommand text
