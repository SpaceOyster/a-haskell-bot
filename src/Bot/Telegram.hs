{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Telegram
    ( module Bot
    , doBotThing
    , withHandle
    , mergeStrings
    , Config(..)
    ) where

import qualified API
import qualified API.Telegram as TG
import API.Telegram.Types
import Bot hiding (strings)
import qualified Bot (strings)
import Control.Exception (finally)
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (IORef, newIORef)
import Data.List.Extended (replaceSubseq)
import qualified Data.Map as Map
    ( Map
    , alter
    , findWithDefault
    , fromList
    , keys
    , lookup
    , mapKeys
    )
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))
import qualified Logger
import System.Environment (getEnv)
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        }
    deriving (Show)

mergeStrings :: Config -> Bot.Strings -> Config
mergeStrings cfg ss = cfg {strings = strings cfg <> ss}

new :: Config -> Logger.Handle -> IO (Handle IO)
new cfg@Config {..} hLog = do
    Logger.info' hLog "Initiating Telegram Bot"
    Logger.debug' hLog $ "Telegram Bot config: " <> show cfg
    state <- newIORef $ BotState {userSettings = mempty}
    hAPI <- TG.new TG.Config {..} hLog
    pure $ Handle {..}

withHandle :: Config -> Logger.Handle -> (Handle IO -> IO a) -> IO a
withHandle config hLog io = do
    hBot <- new config hLog
    io hBot

doBotThing :: Handle IO -> IO [L8.ByteString]
doBotThing hBot@Handle {hLog} = do
    updates <- fetchUpdates hBot
    requests <- reactToUpdates hBot updates
    Logger.info' hLog $
        "Telegram: sending " <> show (length requests) <> " responses"
    mapM (hBot & hAPI & API.sendRequest) requests

fetchUpdates :: Handle IO -> IO [Update]
fetchUpdates hBot@Handle {hLog} = do
    Logger.info' hLog "Telegram: fetching Updates"
    req <- hBot & hAPI & TG.getUpdates
    json <- hBot & hAPI & API.sendRequest $ req
    Logger.debug' hLog "Telegram: decoding json response"
    resp <- throwDecode json
    extractUpdates resp

getUserMultiplier :: Handle m -> User -> IO Int
getUserMultiplier hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = Bot.echoMultiplier hBot
        uhash = hashUser user
        repeats = Map.findWithDefault drepeats uhash $ userSettings st
    pure repeats

getUserMultiplierM :: Handle m -> Maybe User -> IO Int
getUserMultiplierM hBot (Just u) = hBot & getUserMultiplier $ u
getUserMultiplierM hBot@Handle {hLog} Nothing = do
    Logger.warning'
        hLog
        "Telegram: No User info, returning default echo multiplier"
    pure $ hBot & Bot.echoMultiplier

setUserMultiplier :: Handle m -> User -> Int -> IO ()
setUserMultiplier hBot@Handle {hLog} user repeats = do
    Logger.debug' hLog $ "Setting echo multiplier to: " <> show repeats
    Logger.debug' hLog $ "    For User: " <> show user
    hBot `Bot.hSetState` \st ->
        let uhash = hashUser user
            usettings = Map.alter (const $ Just repeats) uhash $ userSettings st
         in st {userSettings = usettings}

reactToUpdates :: Handle IO -> [Update] -> IO [API.Request]
reactToUpdates hBot@Handle {hLog} updates = do
    Logger.info' hLog "Telegram: processing each update"
    requests <- join <$> mapM (reactToUpdate hBot) updates
    pure requests `finally` remember updates
  where
    remember [] = pure ()
    remember us = TG.rememberLastUpdate (hAPI hBot) $ last us

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

reactToUpdate :: Handle IO -> Update -> IO [API.Request]
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

reactToCommand :: Handle IO -> Message -> IO API.Request
reactToCommand hBot@Handle {hLog} msg@Message {message_id} = do
    cmd <- getCommandThrow msg
    Logger.debug' hLog $
        "Telegram: Got command" <> cmd <> " in update id " <> show message_id
    action <- getActionThrow cmd
    runAction action hBot msg

reactToMessage :: Handle IO -> Message -> IO [API.Request]
reactToMessage hBot@Handle {hLog} msg@Message {message_id} = do
    author <- getAuthorThrow msg
    n <- hBot `getUserMultiplier` author
    Logger.debug' hLog $
        "Telegram: generating " <>
        show n <> " echoes for Message: " <> show message_id
    n `replicateM` TG.copyMessage msg

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

reactToCallback :: Handle IO -> CallbackQuery -> IO API.Request
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

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
data Command
    = Start
    | Help
    | Repeat
    | UnknownCommand
    deriving (Show, Enum, Bounded)

newtype Action m =
    Action
        { runAction :: Handle m -> Message -> m API.Request
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: Map.Map BotCommand (Action IO)
commands =
    Map.fromList
        [ ( BotCommand {command = "start", description = "Greet User"}
          , Action
                (\Handle {strings} Message {chat} -> do
                     let address = (chat :: Chat) & chat_id
                     TG.sendMessage address $ Bot.greeting strings))
        , ( BotCommand {command = "help", description = "Show help text"}
          , Action
                (\Handle {strings} Message {chat} -> do
                     let address = (chat :: Chat) & chat_id
                     TG.sendMessage address $ Bot.help strings))
        , ( BotCommand {command = "repeat", description = "Set echo multiplier"}
          , Action
                (\hBot@Handle {strings} Message {chat, from} -> do
                     prompt <- hBot & repeatPrompt $ from
                     let address = (chat :: Chat) & chat_id
                     TG.sendInlineKeyboard address prompt repeatKeyboard))
        ]

repeatPrompt :: Handle m -> Maybe User -> IO String
repeatPrompt hBot userM = do
    mult <- hBot & getUserMultiplierM $ userM
    let prompt' = hBot & Bot.strings & Bot.repeat
    pure $ replaceSubseq prompt' "%n" (show mult)

getActionThrow :: (MonadThrow m) => String -> m (Action IO)
getActionThrow cmd =
    case Map.lookup cmd $ command `Map.mapKeys` commands of
        Just a -> pure a
        Nothing -> throwM $ Ex Priority.Info $ "Unknown command: " ++ cmd

commandsList :: [String]
commandsList = command <$> Map.keys (commands :: Map.Map BotCommand (Action IO))

repeatKeyboard :: InlineKeyboardMarkup
repeatKeyboard =
    InlineKeyboardMarkup [[button 1, button 2, button 3, button 4, button 5]]
  where
    button x =
        InlineKeyboardButton
            {text = show x, callback_data = "repeat_" ++ show x}

isKnownCommand :: String -> Bool
isKnownCommand s = tail s `elem` commandsList

isCommandE :: Message -> Bool
isCommandE Message {text} =
    case text of
        Just t -> isCommand t && isKnownCommand t
        Nothing -> False
