{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API.Telegram where

import API.Telegram.Types

import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (Value(..), (.=), encode, object, toJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.Map as Map (Map, fromList, keys, lookup, mapKeys)
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))
import qualified HTTP
import Network.HTTP.Client as HTTP (RequestBody(..), responseBody)
import System.Environment (getEnv)
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , baseURL :: String
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        }

data Handle m =
    Handle
        { http :: HTTP.Handle m
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        }

new :: Config -> IO (Handle IO)
new cfg@Config {baseURL, helpMessage, greeting, repeatPrompt} = do
    let httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    http <- HTTP.new httpConfig
    return $ Handle {http, helpMessage, greeting, repeatPrompt}

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        helpMessage = "This is a help message"
        greeting = "This is greeting message"
        repeatPrompt = "How many times you want your messages to be repeated?"
    return $ Config {key, baseURL, helpMessage, greeting, repeatPrompt}

getUpdates :: (MonadThrow m) => Handle m -> m [Update]
getUpdates handle = do
    let req = handle & http & HTTP.getRequest $ "getUpdates"
    res <- handle & http & HTTP.sendRequest $ req
    let json = responseBody res
    res' <- throwDecode json
    getResult res'

copyMessage :: (Monad m) => Handle m -> Message -> m L8.ByteString
copyMessage handle msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
        req =
            (handle & http & HTTP.postRequest $ "copyMessage") . RequestBodyLBS $
            json
    res <- handle & http & HTTP.sendRequest $ req
    return $ responseBody res

withHandle :: (Handle IO -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    handle <- new config
    io handle

reactToUpdate :: (MonadThrow m) => Handle m -> Update -> m L8.ByteString
reactToUpdate handle update = do
    msg <- getMessageThrow update
    t <- getTextThrow msg
    if isCommand t && isKnownCommand t
        then reactToCommand handle msg
        else reactToMessage handle msg

reactToCommand :: (MonadThrow m) => Handle m -> Message -> m L8.ByteString
reactToCommand handle msg = do
    cmd <- getCommandThrow msg
    action <- getActionThrow cmd
    runAction action handle msg

reactToMessage :: (Monad m) => Handle m -> Message -> m L8.ByteString
reactToMessage = copyMessage

reactToUpdates :: (MonadThrow m) => Handle m -> [Update] -> m [L8.ByteString]
reactToUpdates handle updates = mapM (reactToUpdate handle) updates

isKnownCommand :: String -> Bool
isKnownCommand s = tail s `elem` commandsList

newtype Action m =
    Action
        { runAction :: (Handle m -> Message -> m L8.ByteString)
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: (Monad m) => Map BotCommand (Action m)
commands =
    Map.fromList $
    [ ( BotCommand {command = "start", description = "Greet User"}
      , Action
            (\h@Handle {greeting} Message {chat} ->
                 sendMessage h (chat & (chat_id :: Chat -> Integer)) $ greeting))
    , ( BotCommand {command = "help", description = "Show help text"}
      , Action
            (\h@Handle {helpMessage} Message {chat} ->
                 sendMessage h (chat & (chat_id :: Chat -> Integer)) $
                 helpMessage))
    , ( BotCommand
            { command = "repeat"
            , description = "Set number of message repeats to make"
            }
      , Action
            (\h@Handle {repeatPrompt} Message {chat} ->
                 sendInlineKeyboard h (chat & (chat_id :: Chat -> Integer)) $
                 repeatPrompt))
    ]

getActionThrow :: (MonadThrow m) => String -> m (Action m)
getActionThrow cmd =
    case Map.lookup cmd $ command `mapKeys` commands of
        Just a -> return a
        Nothing -> throwM $ Ex Priority.Info $ "Unknown command: " ++ cmd

commandsList :: [String]
commandsList = command <$> keys (commands :: Map BotCommand (Action Maybe))

sendMessage :: (Monad m) => Handle m -> Integer -> String -> m L8.ByteString
sendMessage handle chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
        req =
            (handle & http & HTTP.postRequest $ "sendMessage") . RequestBodyLBS $
            json
    res <- handle & http & HTTP.sendRequest $ req
    return $ responseBody res

repeatKeyboard :: InlineKeyboardMarkup
repeatKeyboard =
    InlineKeyboardMarkup [[button 1, button 2, button 3, button 4, button 5]]
  where
    button x = InlineKeyboardButton {text = show x, callback_data = show x}

sendInlineKeyboard ::
       (Monad m) => Handle m -> Integer -> String -> m L8.ByteString
sendInlineKeyboard handle chatId prompt = do
    let json =
            encode . object $
            [ "chat_id" .= chatId
            , "text" .= prompt
            , "reply_markup" .= repeatKeyboard
            ]
        req =
            (handle & http & HTTP.postRequest $ "sendMessage") . RequestBodyLBS $
            json
    res <- handle & http & HTTP.sendRequest $ req
    return $ responseBody res
