{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Telegram where

import API.Telegram.Types

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.Map (Map, fromList, keys, lookup, mapKeys)
import qualified HTTP
import Network.HTTP.Client as HTTP (RequestBody(..), responseBody)
import System.Environment (getEnv)
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , baseURL :: String
        }

data Handle m =
    Handle
        { http :: HTTP.Handle m
        }

new :: Config -> IO (Handle IO)
new cfg = do
    let httpConfig = HTTP.Config {HTTP.baseURL = baseURL cfg}
    httpHandle <- HTTP.new httpConfig
    return $ Handle {http = httpHandle}

parseConfig :: IO Config
parseConfig = do
    k <- getEnv "TG_API"
    let bURL = "https://api.telegram.org/bot" ++ k ++ "/"
    return $ Config {key = k, baseURL = bURL}

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
    if isCommand msg && isKnownCommand msg
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
isKnownCommand s = s `elem` commandsList

newtype Action m =
    Action
        { runAction :: (Handle m -> Message -> m L8.ByteString)
        }

-- | command has to be between 1-32 chars long
-- description hat to be between 3-256 chars long
commands :: (Monad m) => Map BotCommand (Action m)
commands =
    fromList $
    [ ( BotCommand
            {command = "start", description = "Start interacting with bot"}
      , Action echoMessage)
    , ( BotCommand {command = "help", description = "Show help text"}
      , Action echoMessage)
    , ( BotCommand
            { command = "repeat"
            , description = "Set number of message repeats to make"
            }
      , Action echoMessage)
    ]

getActionThrow :: (MonadThrow m) => String -> m (Action n)
getActionThrow = do
    undefined
commandsList :: [String]
commandsList = command <$> keys (commands :: Map BotCommand (Action Maybe))
