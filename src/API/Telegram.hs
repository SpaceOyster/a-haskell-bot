{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API.Telegram where

import API
import API.Telegram.Types

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (Value(..), (.=), encode, object)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.Map as Map (Map, fromList, keys, lookup, mapKeys)
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

new :: Config -> IO (Handle IO)
new cfg@Config {key, helpMessage, greeting, repeatPrompt} = do
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    http <- HTTP.new httpConfig
    return $
        Handle
            { http
            , helpMessage
            , greeting
            , repeatPrompt
            , getUpdates = GET "getUpdates"
            }

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    let helpMessage = "This is a help message"
        greeting = "This is greeting message"
        repeatPrompt = "How many times you want your messages to be repeated?"
    return $ Config {key, helpMessage, greeting, repeatPrompt}

copyMessage :: (Monad m) => Message -> m API.Request
copyMessage msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    return $ POST "copyMessage" json

withHandle :: (Handle IO -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    hAPI <- new config
    io hAPI

reactToUpdate :: (MonadThrow m) => Handle m -> Update -> m API.Request
reactToUpdate hAPI update = do
    msg <- getMessageThrow update
    t <- getTextThrow msg
    if isCommand t && isKnownCommand t
        then reactToCommand hAPI msg
        else reactToMessage msg

reactToCommand :: (MonadThrow m) => Handle m -> Message -> m API.Request
reactToCommand hAPI msg = do
    cmd <- getCommandThrow msg
    action <- getActionThrow cmd
    runAction action hAPI msg

reactToMessage :: (Monad m) => Message -> m API.Request
reactToMessage = copyMessage

reactToUpdates :: (MonadThrow m) => Handle m -> L8.ByteString -> m [API.Request]
reactToUpdates hAPI json = do
    resp <- throwDecode json
    updates <- extractUpdates resp
    mapM (reactToUpdate hAPI) updates

isKnownCommand :: String -> Bool
isKnownCommand s = tail s `elem` commandsList

newtype Action m =
    Action
        { runAction :: Handle m -> Message -> m API.Request
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: (Monad m) => Map BotCommand (Action m)
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

getActionThrow :: (MonadThrow m) => String -> m (Action m)
getActionThrow cmd =
    case Map.lookup cmd $ command `mapKeys` commands of
        Just a -> return a
        Nothing -> throwM $ Ex Priority.Info $ "Unknown command: " ++ cmd

commandsList :: [String]
commandsList = command <$> keys (commands :: Map BotCommand (Action Maybe))

sendMessage :: (Monad m) => Integer -> String -> m API.Request
sendMessage chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    return $ POST "sendMessage" json

repeatKeyboard :: InlineKeyboardMarkup
repeatKeyboard =
    InlineKeyboardMarkup [[button 1, button 2, button 3, button 4, button 5]]
  where
    button x = InlineKeyboardButton {text = show x, callback_data = show x}

sendInlineKeyboard :: (Monad m) => Integer -> String -> m API.Request
sendInlineKeyboard chatId prompt = do
    let json =
            encode . object $
            [ "chat_id" .= chatId
            , "text" .= prompt
            , "reply_markup" .= repeatKeyboard
            ]
    return $ POST "sendMessage" json
