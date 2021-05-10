{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API.Telegram where

import API
import API.Telegram.Types
import Control.Applicative ((<|>))
import Control.Exception (bracket, finally)
import Control.Monad (join, replicateM)
import Data.IORef (newIORef)

import Control.Monad.Catch (MonadCatch, MonadThrow(..), handleAll)
import Data.Aeson (Value(..), (.=), encode, object)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.Map as Map
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
import qualified HTTP
import System.Environment (getEnv)
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        }

data HState =
    HState
        { lastUpdate :: Integer
        , userSettings :: Map.Map User Int
        }

new :: Config -> IO (Handle IO HState)
new cfg@Config {key, helpMessage, greeting, repeatPrompt} = do
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    http <- HTTP.new httpConfig
    state <- newIORef $ HState {lastUpdate = 0, userSettings = mempty}
    return $
        Handle
            { http
            , state
            , helpMessage
            , greeting
            , repeatPrompt
            , defaultRepeat = 1
            , fetchUpdates = GET "getUpdates"
            }

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    let helpMessage = "This is a help message"
        greeting = "This is greeting message"
        repeatPrompt = "How many times you want your messages to be repeated?"
    return $ Config {key, helpMessage, greeting, repeatPrompt}

withHandle :: (Handle IO HState -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    hAPI <- new config
    io hAPI

getLastUpdateID :: Handle m HState -> IO Integer
getLastUpdateID hAPI = do
    st <- hGetState hAPI
    return $ lastUpdate st

setLastUpdateID :: Handle m HState -> Integer -> IO ()
setLastUpdateID hAPI id = do
    hAPI `hSetState` \st -> st {lastUpdate = id}

getUserSettings :: Handle m HState -> User -> IO Int
getUserSettings hAPI user = do
    st <- hGetState hAPI
    let drepeats = defaultRepeat hAPI
        repeats = Map.findWithDefault drepeats user $ userSettings st
    return repeats

setUserSettings :: Handle m HState -> User -> Int -> IO ()
setUserSettings hAPI user repeats = do
    hAPI `hSetState` \st ->
        let usettings = Map.alter (const $ Just repeats) user $ userSettings st
         in st {userSettings = usettings}

rememberLastUpdate :: Handle m HState -> Update -> IO ()
rememberLastUpdate hAPI u = hAPI `setLastUpdateID` (update_id u + 1)

getUpdates :: Handle IO HState -> IO API.Request
getUpdates hAPI =
    bracket (getLastUpdateID hAPI) (const $ return ()) $ \id ->
        let json = encode . object $ ["offset" .= id]
         in return $ POST "getUpdates" json

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

isCommandE :: Message -> Bool
isCommandE Message {text} =
    case text of
        Just t -> isCommand t && isKnownCommand t
        Nothing -> False

reactToUpdate :: Handle IO HState -> Update -> IO [API.Request]
reactToUpdate hAPI update = do
    let qu = qualifyUpdate update
    case qu of
        ECommand msg -> (: []) <$> reactToCommand hAPI msg
        EMessage msg -> reactToMessage hAPI msg
        ECallback cq -> (: []) <$> reactToCallback hAPI cq
        EOther Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id

reactToCommand :: Handle IO state -> Message -> IO API.Request
reactToCommand hAPI msg = do
    cmd <- getCommandThrow msg
    action <- getActionThrow cmd
    runAction action hAPI msg

reactToMessage :: Handle IO HState -> Message -> IO [API.Request]
reactToMessage hAPI msg = do
    author <- getAuthorThrow msg
    n <- hAPI `getUserSettings` author
    n `replicateM` copyMessage msg

data QueryData
    = QDRepeat Int
    | QDOther String
    deriving (Show)

copyMessage :: (Monad m) => Message -> m API.Request
copyMessage msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    return $ POST "copyMessage" json

reactToUpdates :: Handle IO HState -> L8.ByteString -> IO [Request]
reactToUpdates hAPI json = do
    resp <- throwDecode json
    updates <- extractUpdates resp
    requests <- mapM (reactToUpdate hAPI) updates
    return (join requests) `finally` remember updates
  where
    remember [] = return ()
    remember us = rememberLastUpdate hAPI $ last us

isKnownCommand :: String -> Bool
isKnownCommand s = tail s `elem` commandsList

newtype Action m state =
    Action
        { runAction :: Handle m state -> Message -> m API.Request
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: (Monad m) => Map BotCommand (Action m state)
commands =
    Map.fromList
        [ ( BotCommand {command = "start", description = "Greet User"}
          , Action
                (\Handle {greeting} Message {chat} ->
                     sendMessage ((chat :: Chat) & chat_id) greeting))
        , ( BotCommand {command = "help", description = "Show help text"}
          , Action
                (\Handle {helpMessage} Message {chat} ->
                     sendMessage ((chat :: Chat) & chat_id) helpMessage))
        , ( BotCommand
                { command = "repeat"
                , description = "Set number of message repeats to make"
                }
          , Action
                (\Handle {repeatPrompt} Message {chat} ->
                     sendInlineKeyboard ((chat :: Chat) & chat_id) repeatPrompt))
        ]

getActionThrow :: (MonadThrow m) => String -> m (Action m state)
getActionThrow cmd =
    case Map.lookup cmd $ command `mapKeys` commands of
        Just a -> return a
        Nothing -> throwM $ Ex Priority.Info $ "Unknown command: " ++ cmd

commandsList :: [String]
commandsList = command <$> keys (commands :: Map BotCommand (Action Maybe ()))

sendMessage :: (Monad m) => Integer -> String -> m API.Request
sendMessage chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    return $ POST "sendMessage" json

repeatKeyboard :: InlineKeyboardMarkup
repeatKeyboard =
    InlineKeyboardMarkup [[button 1, button 2, button 3, button 4, button 5]]
  where
    button x =
        InlineKeyboardButton
            {text = show x, callback_data = "repeat_" ++ show x}

sendInlineKeyboard :: (Monad m) => Integer -> String -> m API.Request
sendInlineKeyboard chatId prompt = do
    let json =
            encode . object $
            [ "chat_id" .= chatId
            , "text" .= prompt
            , "reply_markup" .= repeatKeyboard
            ]
    return $ POST "sendMessage" json
