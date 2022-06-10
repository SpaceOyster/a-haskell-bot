{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Telegram.MethodsSpec (spec) where

import API.Telegram.Methods as TG
  ( MessageID,
    answerCallbackQuery,
    answerCallbackQuery_,
    copyMessage,
    copyMessage_,
    getUpdates,
    getUpdates_,
    sendInlineKeyboard,
    sendInlineKeyboard_,
    sendMessage,
    sendMessage_,
  )
import API.Telegram.Monad as TG (Config (..), TGState (..), TelegramT (..), evalTelegramT, putTGState)
import API.Telegram.Types as TG
  ( Chat (Chat, chat_id),
    Error,
    InlineKeyboardMarkup,
    Message (Message, chat, date, from, message_id, reply_markup, text),
    Update,
    User,
  )
import App.Env as App
  ( Env (Env, envBotReplies, envHTTP, envLogger, envUsersDB),
  )
import App.Monad as App (App, AppEnv, evalApp)
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad.Reader (MonadIO (..))
import Data.Aeson as Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import qualified Effects.BotReplies as BR
import Effects.HTTP as HTTP (Request (POST))
import Effects.Log as Log (MonadLog (..))
import Network.URI as URI (URI (uriPath))
import Test.App.Error as App (isAPIError)
import Test.Arbitrary.Telegram.Types ()
import Test.Arbitrary.Text (AnyText (AnyText), ShortCleanText (ShortCleanText))
import qualified Test.Handlers.HTTP as HTTP
import qualified Test.Handlers.Logger as Logger
import qualified Test.Handlers.UsersDB as DB
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldReturn,
    shouldThrow,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, generate, resize)

spec :: Spec
spec = do
  getUpdatesSpec
  answerCallbackQuerySpec
  copyMessageSpec
  sendMessageSpec
  sendInlineKeyboardSpec
  getUpdatesSpec_
  answerCallbackQuerySpec_
  copyMessageSpec_
  sendMessageSpec_
  sendInlineKeyboardSpec_

emptyReplies :: BR.Replies
emptyReplies = BR.Replies "" "" "" "" ""

httpTestEnv :: (MonadIO m) => L8.ByteString -> m App.AppEnv
httpTestEnv bs = do
  db <- DB.new
  pure
    Env
      { envLogger = Logger.new,
        envHTTP = HTTP.new bs,
        envUsersDB = db,
        envBotReplies = emptyReplies
      }

modelHTTPReply :: (MonadIO m) => Config -> L8.ByteString -> TelegramT App a -> m a
modelHTTPReply apiCfg replyBS action = do
  env <- httpTestEnv replyBS
  liftIO $ App.evalApp env $ evalTelegramT apiCfg action

instance Log.MonadLog Maybe where
  doLog _ _ = pure ()

instance MonadCatch Maybe where
  catch x _h = x

instance Log.MonadLog IO where
  doLog _ _ = pure ()

getUpdatesSpec :: Spec
getUpdatesSpec = describe "getUpdates" $ do
  prop "returns [Updates] on success" $ \(tgConfig, updates) -> do
    let response = encode $ object ["ok" .= True, "result" .= updates]
    modelHTTPReply tgConfig response getUpdates
      `shouldReturn` (updates :: [Update])
  prop "throws on Error" $ \(tgConfig, err) -> do
    let response = encode (err :: TG.Error)
    modelHTTPReply tgConfig response getUpdates
      `shouldThrow` App.isAPIError
  prop "throws on unexpected Response" $ \(tgConfig, randomResponse) -> do
    let response = L8.pack randomResponse
    modelHTTPReply tgConfig response getUpdates
      `shouldThrow` App.isAPIError

answerCallbackQuerySpec :: Spec
answerCallbackQuerySpec = describe "answerCallbackQuerySpec" $ do
  prop "returns True on success" $ \tgConfig -> do
    let response = "{\"ok\":true,\"result\":true}"
    modelHTTPReply tgConfig response (answerCallbackQuery "query")
      `shouldReturn` True
  prop "throws on Error" $ \(tgConfig, err) -> do
    let response = encode (err :: TG.Error)
    modelHTTPReply tgConfig response (answerCallbackQuery "query")
      `shouldThrow` App.isAPIError
  prop "throws on unexpected Response" $ \(tgConfig, randomResponse) -> do
    let response = L8.pack randomResponse
    modelHTTPReply tgConfig response (answerCallbackQuery "query")
      `shouldThrow` App.isAPIError

copyMessageSpec :: Spec
copyMessageSpec = describe "copyMessageSpec" $ do
  prop "returns JSON with only `message_id :: Integer` field" $
    \(tgConfig, tgMsg) -> do
      resultMsgId <- generate $ resize 10 (arbitrary :: Gen MessageID)
      let result =
            encode . object $
              [ "ok" .= True,
                "result" .= resultMsgId
              ]
      modelHTTPReply tgConfig result (copyMessage tgMsg)
        `shouldReturn` resultMsgId
  prop "throws on Error" $ \(tgConfig, err) -> do
    tgMsg <- generate (arbitrary :: Gen TG.Message)
    let response = encode (err :: TG.Error)
    modelHTTPReply tgConfig response (copyMessage tgMsg)
      `shouldThrow` App.isAPIError
  prop "throws on unexpected Response" $ \(tgConfig, randomResponse) -> do
    tgMsg <- generate (arbitrary :: Gen TG.Message)
    let response = L8.pack randomResponse
    modelHTTPReply tgConfig response (copyMessage tgMsg)
      `shouldThrow` App.isAPIError

sendMessageSpec :: Spec
sendMessageSpec = describe "sendMessageSpec" $ do
  context "given Chat ID and Text, sends text message" $
    prop "returns Message on success" $ \(tgConfig, AnyText msgText) -> do
      chat <- generate (arbitrary :: Gen TG.Chat)
      message_id <- generate (arbitrary :: Gen Integer)
      date <- generate (arbitrary :: Gen Integer)
      reply_markup <- generate (arbitrary :: Gen (Maybe TG.InlineKeyboardMarkup))
      from <- generate (arbitrary :: Gen (Maybe TG.User))
      let tgMsg =
            TG.Message
              { message_id,
                from,
                chat,
                date,
                TG.text = Just msgText,
                reply_markup
              }
          response =
            encode . object $
              ["ok" .= True, "result" .= (tgMsg :: TG.Message)]
      modelHTTPReply tgConfig response (sendMessage (TG.chat_id chat) msgText)
        `shouldReturn` (tgMsg :: TG.Message)
  prop "throws on Error" $ \(tgConfig, err) -> do
    chatId <- generate (arbitrary :: Gen Integer)
    AnyText msgText <- generate arbitrary
    let response = encode (err :: TG.Error)
    modelHTTPReply tgConfig response (sendMessage chatId msgText)
      `shouldThrow` App.isAPIError
  prop "throws on unexpected Response" $ \(tgConfig, randomResponse) -> do
    chatId <- generate (arbitrary :: Gen Integer)
    AnyText msgText <- generate arbitrary
    let response = L8.pack randomResponse
    modelHTTPReply tgConfig response (sendMessage chatId msgText)
      `shouldThrow` App.isAPIError

sendInlineKeyboardSpec :: Spec
sendInlineKeyboardSpec = describe "sendInlineKeyboardSpec" $ do
  context "given Chat ID, Text and KeyboardLayout, sends inline keyboard message" $
    prop "returns Message on success" $
      \(tgConfig, chatId, AnyText promptText, kbdMarkup) -> do
        message_id <- generate (arbitrary :: Gen Integer)
        date <- generate (arbitrary :: Gen Integer)
        from <- generate (arbitrary :: Gen (Maybe TG.User))
        let resultMsg =
              TG.Message
                { message_id,
                  from,
                  TG.chat = Chat chatId,
                  date,
                  TG.text = Just promptText,
                  TG.reply_markup = Just kbdMarkup
                }
            response = encode . object $ ["ok" .= True, "result" .= resultMsg]
        modelHTTPReply tgConfig response (sendInlineKeyboard chatId promptText kbdMarkup)
          `shouldReturn` resultMsg
  prop "throws on Error" $ \(tgConfig, err) -> do
    chatId <- generate (arbitrary :: Gen Integer)
    AnyText promptText <- generate arbitrary
    kbdMarkup <- generate (arbitrary :: Gen InlineKeyboardMarkup)
    let response = encode (err :: TG.Error)
    modelHTTPReply tgConfig response (sendInlineKeyboard chatId promptText kbdMarkup)
      `shouldThrow` App.isAPIError
  prop "throws on unexpected Response" $ \(tgConfig, randomResponse) -> do
    chatId <- generate (arbitrary :: Gen Integer)
    AnyText promptText <- generate arbitrary
    kbdMarkup <- generate (arbitrary :: Gen InlineKeyboardMarkup)
    let response = L8.pack randomResponse
    modelHTTPReply tgConfig response (sendInlineKeyboard chatId promptText kbdMarkup)
      `shouldThrow` App.isAPIError

getUpdatesSpec_ :: Spec
getUpdatesSpec_ = describe "getUpdates_" $ do
  prop "returns Effects.HTTP.Request for \"getUpdates\" Telegram API method" $
    \(tgConfig, tgState) -> do
      let apiURI_ = TG.apiURI tgState
          uri = apiURI_ {URI.uriPath = URI.uriPath apiURI_ <> "getUpdates"}
          json =
            encode $
              object
                [ "offset" .= TG.lastUpdate tgState,
                  "timeout" .= TG.timeout tgState
                ]
      evalTelegramT tgConfig (putTGState tgState >> getUpdates_) `shouldReturn` HTTP.POST uri json

answerCallbackQuerySpec_ :: Spec
answerCallbackQuerySpec_ = describe "answerCallbackQuerySpec_" $
  context "given callback query id" $
    prop "returns Effects.HTTP.Request for \"answerCallbackQuery\" Telegram API method" $
      \(tgConfig, tgState, ShortCleanText cbQuery) -> do
        let apiURI_ = TG.apiURI tgState
            uri = apiURI_ {URI.uriPath = URI.uriPath apiURI_ <> "answerCallbackQuery"}
            json = encode $ object ["callback_query_id" .= cbQuery]
        evalTelegramT tgConfig (putTGState tgState >> answerCallbackQuery_ cbQuery)
          `shouldReturn` HTTP.POST uri json

copyMessageSpec_ :: Spec
copyMessageSpec_ = describe "copyMessageSpec_" $
  context "given a Message" $
    prop "returns Effects.HTTP.Request for \"copyMessage\" Telegram API method" $
      \(tgConfig, tgState, tgMsg) -> do
        let apiURI_ = TG.apiURI tgState
            uri = apiURI_ {URI.uriPath = URI.uriPath apiURI_ <> "copyMessage"}
            chatId = TG.chat_id (TG.chat tgMsg)
            json =
              encode $
                object
                  [ "from_chat_id" .= chatId,
                    "message_id" .= TG.message_id tgMsg,
                    "chat_id" .= chatId
                  ]
        evalTelegramT tgConfig (putTGState tgState >> copyMessage_ tgMsg)
          `shouldReturn` HTTP.POST uri json

sendMessageSpec_ :: Spec
sendMessageSpec_ = describe "sendMessageSpec_" $
  context "given chat_id and Text" $
    prop "returns Effects.HTTP.Request for \"sendMessage\" Telegram API method" $
      \(tgConfig, tgState, TG.Chat chatId, AnyText msgText) -> do
        let apiURI_ = TG.apiURI tgState
            uri = apiURI_ {URI.uriPath = URI.uriPath apiURI_ <> "sendMessage"}
            json =
              encode $
                object ["text" .= msgText, "chat_id" .= chatId]
        evalTelegramT tgConfig (putTGState tgState >> sendMessage_ chatId msgText)
          `shouldReturn` HTTP.POST uri json

sendInlineKeyboardSpec_ :: Spec
sendInlineKeyboardSpec_ = describe "sendInlineKeyboardSpec_" $
  context "given chat_id, prompt text and keyboard markup" $
    prop "returns Effects.HTTP.Request for \"sendInlineKeyboard\" Telegram API method" $ do
      \(tgConfig, tgState, TG.Chat chatId, AnyText msgText, kbdMarkup) -> do
        let apiURI_ = TG.apiURI tgState
            uri = apiURI_ {URI.uriPath = URI.uriPath apiURI_ <> "sendMessage"}
            json =
              encode $
                object
                  [ "text" .= msgText,
                    "chat_id" .= chatId,
                    "reply_markup" .= (kbdMarkup :: TG.InlineKeyboardMarkup)
                  ]
        evalTelegramT tgConfig (putTGState tgState >> sendInlineKeyboard_ chatId msgText kbdMarkup)
          `shouldReturn` HTTP.POST uri json
