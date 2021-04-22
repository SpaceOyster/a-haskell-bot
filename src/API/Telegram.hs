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
        }

data Handle m =
    Handle
        { http :: HTTP.Handle m
        , helpMessage :: String
        , greeting :: String
        }

new :: Config -> IO (Handle IO)
new cfg@Config {baseURL, helpMessage, greeting} = do
    let httpConfig = HTTP.Config {HTTP.baseURL = baseURL}
    httpHandle <- HTTP.new httpConfig
    return $
        Handle
            {http = httpHandle, helpMessage = helpMessage, greeting = greeting}

parseConfig :: IO Config
parseConfig = do
    key <- getEnv "TG_API"
    let baseURL = "https://api.telegram.org/bot" ++ key ++ "/"
        helpMessage = "This is a help message"
        greeting = "This is greeting message"
    return $ Config {key, baseURL, helpMessage, greeting}

getUpdates :: (MonadThrow m) => Handle m -> m [Update]
getUpdates handle = do
    let req = handle & http & HTTP.getRequest $ "getUpdates"
    res <- handle & http & HTTP.sendRequest $ req
    let json = responseBody res
    res' <- throwDecode json
    getResult res'

echoMessage :: (Monad m) => Handle m -> Message -> m L8.ByteString
echoMessage handle msg = do
    let req =
            (handle & http & HTTP.postRequest $ "copyMessage") .
            RequestBodyLBS . encode . copyMessage $
            msg
    res <- handle & http & HTTP.sendRequest $ req
    return $ responseBody res

withHandle :: (Handle IO -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    handle <- new config
    io handle

echoAll :: (MonadThrow m) => Handle m -> m [L8.ByteString]
echoAll handle = do
    updates <- getUpdates handle -- add error handling
    mapM (echoMessage handle) $ message <$> updates

reactToUpdate :: (MonadThrow m) => Handle m -> Update -> m L8.ByteString
reactToUpdate handle update = do
    let msg = message update
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
reactToMessage = echoMessage

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
      , Action echoMessage)
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
    InlineKeyboardMarkup
        [ [button 1, button 2, button 3]
        , [button 4, button 5, button 6]
        , [button 7, button 8, button 9]
        ]
  where
    button x = InlineKeyboardButton {text = show x, callback_data = show x}

sendInlineKeyboard ::
       (Monad m) => Handle m -> Integer -> String -> m L8.ByteString
sendInlineKeyboard handle chatId msg = do
    let json =
            encode . object $
            [ "chat_id" .= chatId
            , "text" .= msg
            , "reply_markup" .= repeatKeyboard
            ]
        req =
            (handle & http & HTTP.postRequest $ "sendMessage") . RequestBodyLBS $
            json
    res <- handle & http & HTTP.sendRequest $ req
    return $ responseBody res
