{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module API.Telegram
    ( module API
    , new
    , rememberLastUpdate
    , Config(..)
    , APIState(..)
    , Method(..)
    , runMethod
    ) where

import qualified API
import API.Telegram.Types
import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (Value(..), (.=), encode, object)
import Data.Function ((&))
import qualified Exceptions as Ex
import qualified HTTP
import qualified Logger
import qualified Network.URI.Extended as URI

type APIState = Integer

type Handle = API.Handle APIState

newtype Config =
    Config
        { key :: String
        }

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
    maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Telegram API URL"

new :: Config -> Logger.Handle -> IO Handle
new cfg@Config {key} hLog = do
    Logger.info' hLog "Initiating Telegram API handle"
    baseURI <- makeBaseURI cfg
    let httpConfig = HTTP.Config {}
    http <- HTTP.new httpConfig
    Logger.info' hLog "HTTP handle initiated for Telegram API"
    apiState <- newIORef 0
    pure $ API.Handle {..}

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config hLog io = do
    hAPI <- new config hLog
    io hAPI

apiMethod :: Handle -> String -> URI.URI
apiMethod hAPI method = API.baseURI hAPI `URI.addPath` method

rememberLastUpdate :: Handle -> Response -> IO Response
rememberLastUpdate hAPI res@Result {result} =
    case result of
        [] -> pure res
        x -> API.setState hAPI ((1 +) . update_id . last $ result) >> pure res
rememberLastUpdate hAPI err = pure err

data Method
    = GetUpdates
    | AnswerCallbackQuery String
    | CopyMessage Message
    | SendMessage Integer String
    | SendInlineKeyboard Integer String InlineKeyboardMarkup
    deriving (Show)

runMethod :: Handle -> Method -> IO API.Request
runMethod hAPI m =
    case m of
        GetUpdates -> getUpdates hAPI
        AnswerCallbackQuery id -> answerCallbackQuery hAPI id
        CopyMessage msg -> copyMessage hAPI msg
        SendMessage chatId msg -> sendMessage hAPI chatId msg
        SendInlineKeyboard chatId prompt keyboard ->
            sendInlineKeyboard hAPI chatId prompt keyboard

-- API method
getUpdates :: Handle -> IO API.Request
getUpdates hAPI@API.Handle {hLog} =
    bracket (API.getState hAPI) (const $ pure ()) $ \id -> do
        Logger.debug' hLog $ "Telegram: last recieved Update id: " <> show id
        let json = encode . object $ ["offset" .= id, "timeout" .= (25 :: Int)]
        pure $ API.POST (apiMethod hAPI "getUpdates") json

-- API method
answerCallbackQuery :: (Monad m) => Handle -> String -> m API.Request
answerCallbackQuery hAPI id = do
    let json = encode . object $ ["callback_query_id" .= id]
    pure $ API.POST (apiMethod hAPI "answerCallbackQuery") json

-- API method
copyMessage :: (Monad m) => Handle -> Message -> m API.Request
copyMessage hAPI msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    pure $ API.POST (apiMethod hAPI "copyMessage") json

-- API method
sendMessage :: (Monad m) => Handle -> Integer -> String -> m API.Request
sendMessage hAPI chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    pure $ API.POST (apiMethod hAPI "sendMessage") json

-- API method
sendInlineKeyboard ::
       (Monad m)
    => Handle
    -> Integer
    -> String
    -> InlineKeyboardMarkup
    -> m API.Request
sendInlineKeyboard hAPI chatId prompt keyboard = do
    let json =
            encode . object $
            ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
    pure $ API.POST (apiMethod hAPI "sendMessage") json
