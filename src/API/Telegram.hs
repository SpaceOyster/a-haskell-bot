{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module API.Telegram
    ( Config(..)
    , Method(..)
    , Handle(..)
    , new
    , runMethod
    , module API.Telegram.Types
    ) where

import qualified API.Class as API
import API.Telegram.Types
import Control.Exception (bracket)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson.Extended (Value(..), (.=), encode, object, throwDecode)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Exceptions as Ex
import qualified HTTP
import Handle.Class (IsHandle(..))
import qualified Logger as L
import qualified Network.URI.Extended as URI

type TGState = Integer

data Handle =
    Handle
        { http :: HTTP.Handle
        , hLog :: L.Handle
        , baseURI :: URI.URI
        , apiState :: IORef TGState
        }

instance API.APIHandle Handle

instance L.HasLog Handle where
    getLog Handle {hLog} = \p t -> L.getLog hLog p $ "API.Telegram: " <> t

getState :: Handle -> IO TGState
getState = readIORef . apiState

modifyState :: Handle -> (TGState -> TGState) -> IO ()
modifyState hAPI morph = apiState hAPI `modifyIORef'` morph

setState :: Handle -> TGState -> IO ()
setState hAPI newState = modifyState hAPI $ const newState

get :: Handle -> URI.URI -> IO L8.ByteString
get hAPI = hAPI & http & HTTP.get

post :: Handle -> URI.URI -> L8.ByteString -> IO L8.ByteString
post hAPI = hAPI & http & HTTP.post

sendRequest :: Handle -> HTTP.Request -> IO L8.ByteString
sendRequest hAPI@Handle {hLog} req = do
    L.logDebug hLog $ "Vkontakte: sending request: " <> T.tshow req
    res <-
        case req of
            HTTP.GET method -> get hAPI method
            HTTP.POST method body -> post hAPI method body
    L.logDebug hLog $ "Vkontakte: got response: " <> (T.pack $ L8.unpack res)
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
    new :: Config -> L.Handle -> IO Handle
    new cfg@Config {key} hLog = do
        L.logInfo hLog "Initiating Telegram API handle"
        baseURI <- makeBaseURI cfg
        let httpConfig = HTTP.Config {}
        http <- HTTP.new httpConfig
        L.logInfo hLog "HTTP handle initiated for Telegram API"
        apiState <- newIORef 0
        pure $ Handle {..}

withHandle :: Config -> L.Handle -> (Handle -> IO a) -> IO a
withHandle config hLog io = do
    hAPI <- new config hLog
    io hAPI

apiMethod :: Handle -> String -> URI.URI
apiMethod hAPI method = baseURI hAPI `URI.addPath` method

rememberLastUpdate :: Handle -> Response -> IO Response
rememberLastUpdate hAPI res =
    mapM_ (setState hAPI) (newStateFromM res) >> pure res

newStateFromM :: Response -> Maybe TGState
newStateFromM (Updates us@(_x:_xs)) = Just . (1 +) . update_id . last $ us
newStateFromM _ = Nothing

runMethod :: Handle -> Method -> IO Response
runMethod hAPI m =
    bracket (getState hAPI) (const $ pure ()) $ \state -> do
        L.logDebug (hLog hAPI) $
            "Telegram: last recieved Update id: " <> T.tshow state
        runMethod' hAPI state m >>= sendRequest hAPI >>= throwDecode >>=
            rememberLastUpdate hAPI

data Method
    = GetUpdates
    | AnswerCallbackQuery String
    | CopyMessage Message
    | SendMessage Integer String
    | SendInlineKeyboard Integer String InlineKeyboardMarkup
    deriving (Show)

runMethod' :: (Monad m) => Handle -> TGState -> Method -> m HTTP.Request
runMethod' hAPI s m =
    case m of
        GetUpdates -> getUpdates hAPI s
        AnswerCallbackQuery id -> answerCallbackQuery hAPI id
        CopyMessage msg -> copyMessage hAPI msg
        SendMessage chatId msg -> sendMessage hAPI chatId msg
        SendInlineKeyboard chatId prompt keyboard ->
            sendInlineKeyboard hAPI chatId prompt keyboard

-- API method
getUpdates :: (Monad m) => Handle -> TGState -> m HTTP.Request
getUpdates hAPI id = do
    let json = encode . object $ ["offset" .= id, "timeout" .= (25 :: Int)]
    pure $ HTTP.POST (apiMethod hAPI "getUpdates") json

-- API method
answerCallbackQuery :: (Monad m) => Handle -> String -> m HTTP.Request
answerCallbackQuery hAPI id = do
    let json = encode . object $ ["callback_query_id" .= id]
    pure $ HTTP.POST (apiMethod hAPI "answerCallbackQuery") json

-- API method
copyMessage :: (Monad m) => Handle -> Message -> m HTTP.Request
copyMessage hAPI msg@Message {message_id, chat} = do
    let json =
            encode . object $
            [ "chat_id" .= chat_id (chat :: Chat)
            , "from_chat_id" .= chat_id (chat :: Chat)
            , "message_id" .= message_id
            ]
    pure $ HTTP.POST (apiMethod hAPI "copyMessage") json

-- API method
sendMessage :: (Monad m) => Handle -> Integer -> String -> m HTTP.Request
sendMessage hAPI chatId msg = do
    let json = encode . object $ ["chat_id" .= chatId, "text" .= msg]
    pure $ HTTP.POST (apiMethod hAPI "sendMessage") json

-- API method
sendInlineKeyboard ::
       (Monad m)
    => Handle
    -> Integer
    -> String
    -> InlineKeyboardMarkup
    -> m HTTP.Request
sendInlineKeyboard hAPI chatId prompt keyboard = do
    let json =
            encode . object $
            ["chat_id" .= chatId, "text" .= prompt, "reply_markup" .= keyboard]
    pure $ HTTP.POST (apiMethod hAPI "sendMessage") json
