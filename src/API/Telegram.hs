{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Telegram
    ( module API
    , new
    , Config(..)
    , APIState(..)
    , Method(..)
    , Handle(..)
    , runMethod
    ) where

import qualified API.Class as API
import API.Telegram.Types
import Control.Exception (bracket)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson (Value(..), (.=), encode, object)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import qualified Exceptions as Ex
import qualified HTTP
import Handle.Class (IsHandle(..))
import qualified Logger
import qualified Network.URI.Extended as URI
import Utils (throwDecode)

type APIState = Integer

data Handle =
    Handle
        { http :: HTTP.Handle
        , hLog :: Logger.Handle
        , baseURI :: URI.URI
        , apiState :: IORef API.PollCreds
        }

instance API.APIHandle Handle

getState :: Handle -> IO API.PollCreds
getState = readIORef . apiState

modifyState :: Handle -> (API.PollCreds -> API.PollCreds) -> IO ()
modifyState hAPI morph = apiState hAPI `modifyIORef'` morph

setState :: Handle -> API.PollCreds -> IO ()
setState hAPI newState = modifyState hAPI $ const newState

get :: Handle -> URI.URI -> IO L8.ByteString
get hAPI = hAPI & http & HTTP.get'

post :: Handle -> URI.URI -> L8.ByteString -> IO L8.ByteString
post hAPI = hAPI & http & HTTP.post'

sendRequest :: Handle -> API.Request -> IO L8.ByteString
sendRequest hAPI@Handle {hLog} req = do
    Logger.debug' hLog $ "Vkontakte: sending request: " <> show req
    res <-
        case req of
            API.GET method -> get hAPI method
            API.POST method body -> post hAPI method body
    Logger.debug' hLog $ "Vkontakte: got response: " <> L8.unpack res
    pure res

newtype Config =
    Config
        { key :: String
        }

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
    maybe ex pure . URI.parseURI $ "https://api.telegram.org/bot" <> key <> "/"
  where
    ex = throwM $ Ex.URLParsing "Unable to parse Telegram API URL"

instance IsHandle Handle Config where
    new :: Config -> Logger.Handle -> IO Handle
    new cfg@Config {key} hLog = do
        Logger.info' hLog "Initiating Telegram API handle"
        baseURI <- makeBaseURI cfg
        let httpConfig = HTTP.Config {}
        http <- HTTP.new httpConfig
        Logger.info' hLog "HTTP handle initiated for Telegram API"
        apiState <- newIORef 0
        apiState <-
            newIORef $
            API.PollCreds
                { pollURI = baseURI `URI.addPath` "getUpdates"
                , queryParams = mempty
                , body =
                      encode . object $
                      ["offset" .= (0 :: Int), "timeout" .= (25 :: Int)]
                }
        pure $ Handle {..}

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config hLog io = do
    hAPI <- new config hLog
    io hAPI

apiMethod :: Handle -> String -> URI.URI
apiMethod hAPI method = baseURI hAPI `URI.addPath` method

rememberLastUpdate :: Handle -> Response -> IO Response
rememberLastUpdate hAPI res = modifyState hAPI (updateCredsWith res) >> pure res

newStateFromM :: Response -> Maybe APIState
newStateFromM (Updates us@(_x:_xs)) = Just . (1 +) . update_id . last $ us
newStateFromM _ = Nothing

updateCredsWith :: Response -> (API.PollCreds -> API.PollCreds)
updateCredsWith (Updates us@(_x:_xs)) = \p -> p {API.body = newBody}
  where
    newBody =
        encode . object $
        [ "offset" .= ((1 +) . update_id . last $ us :: Integer)
        , "timeout" .= (25 :: Int)
        ]
updateCredsWith _ = Prelude.id

runMethod :: Handle -> Method -> IO Response
runMethod hAPI m =
    rememberLastUpdate hAPI =<<
    throwDecode =<< sendRequest hAPI =<< runMethod' hAPI m

data Method
    = GetUpdates
    | AnswerCallbackQuery String
    | CopyMessage Message
    | SendMessage Integer String
    | SendInlineKeyboard Integer String InlineKeyboardMarkup
    deriving (Show)

runMethod' :: Handle -> Method -> IO API.Request
runMethod' hAPI m =
    case m of
        GetUpdates -> getUpdates hAPI
        AnswerCallbackQuery id -> answerCallbackQuery hAPI id
        CopyMessage msg -> copyMessage hAPI msg
        SendMessage chatId msg -> sendMessage hAPI chatId msg
        SendInlineKeyboard chatId prompt keyboard ->
            sendInlineKeyboard hAPI chatId prompt keyboard

-- API method
getUpdates :: Handle -> IO API.Request
getUpdates hAPI@Handle {hLog} =
    bracket (getState hAPI) (const $ pure ()) $ \creds -> do
        Logger.debug' hLog $ "Telegram: last recieved Update id: " <> show creds
        pure $ API.credsToRequest creds

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
