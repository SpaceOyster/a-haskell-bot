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
import API.Telegram.Monad (Config (..), TelegramT (..), evalTelegramT)
import API.Telegram.Types as TG
  ( Chat (Chat, chat_id),
    Error,
    InlineKeyboardButton (InlineKeyboardButton),
    InlineKeyboardMarkup (InlineKeyboardMarkup),
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
import Data.Maybe (fromJust)
import qualified Effects.BotReplies as BR
import Effects.HTTP as HTTP (Request (POST))
import Effects.Log as Log (MonadLog (..))
import Network.URI as URI (parseURI)
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
    it,
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

testConfig1 :: Config
testConfig1 = Config "KEY1" 100

testConfig2 :: Config
testConfig2 = Config "KEY2" 22

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
  it "returns Effects.HTTP.Request for \"getUpdates\" Telegram API method" $ do
    evalTelegramT testConfig1 getUpdates_ `shouldReturn` HTTP.POST uri1 json1
    evalTelegramT testConfig2 getUpdates_ `shouldReturn` HTTP.POST uri2 json2
  where
    uri1 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY1/getUpdates"
    json1 = "{\"offset\":0,\"timeout\":100}"
    uri2 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY2/getUpdates"
    json2 = "{\"offset\":0,\"timeout\":22}"

answerCallbackQuerySpec_ :: Spec
answerCallbackQuerySpec_ = describe "answerCallbackQuerySpec_" $
  context "given callback query id" $
    it "returns Effects.HTTP.Request for \"answerCallbackQuery\" Telegram API method" $ do
      evalTelegramT testConfig1 (answerCallbackQuery_ "queryID42")
        `shouldReturn` HTTP.POST uri1 json1
      evalTelegramT testConfig2 (answerCallbackQuery_ "queryID17")
        `shouldReturn` HTTP.POST uri2 json2
  where
    uri1 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY1/answerCallbackQuery"
    json1 = "{\"callback_query_id\":\"queryID42\"}"
    uri2 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY2/answerCallbackQuery"
    json2 = "{\"callback_query_id\":\"queryID17\"}"

copyMessageSpec_ :: Spec
copyMessageSpec_ = describe "copyMessageSpec_" $
  context "given a Message" $
    it "returns Effects.HTTP.Request for \"copyMessage\" Telegram API method" $ do
      evalTelegramT testConfig1 (copyMessage_ msg1)
        `shouldReturn` HTTP.POST uri1 json1
      evalTelegramT testConfig2 (copyMessage_ msg2)
        `shouldReturn` HTTP.POST uri2 json2
  where
    uri1 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY1/copyMessage"
    chat1 = Chat 11
    msg1 =
      Message
        { message_id = 123,
          from = Nothing,
          chat = chat1,
          date = 321,
          text = Just "text1"
        }
    json1 = "{\"from_chat_id\":11,\"message_id\":123,\"chat_id\":11}"
    uri2 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY2/copyMessage"
    chat2 = Chat 222
    msg2 =
      Message
        { message_id = 432,
          from = Nothing,
          chat = chat2,
          date = 321,
          text = Just "text1"
        }
    json2 = "{\"from_chat_id\":222,\"message_id\":432,\"chat_id\":222}"

sendMessageSpec_ :: Spec
sendMessageSpec_ = describe "sendMessageSpec_" $
  context "given chat_id and Text" $
    it "returns Effects.HTTP.Request for \"sendMessage\" Telegram API method" $ do
      evalTelegramT testConfig1 (sendMessage_ 11 "some text1")
        `shouldReturn` HTTP.POST uri1 json1
      evalTelegramT testConfig2 (sendMessage_ 22 "some text2")
        `shouldReturn` HTTP.POST uri2 json2
  where
    uri1 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY1/sendMessage"
    json1 = "{\"text\":\"some text1\",\"chat_id\":11}"
    uri2 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY2/sendMessage"
    json2 = "{\"text\":\"some text2\",\"chat_id\":22}"

sendInlineKeyboardSpec_ :: Spec
sendInlineKeyboardSpec_ = describe "sendInlineKeyboardSpec_" $
  context "given chat_id, prompt text and keyboard markup" $
    it "returns Effects.HTTP.Request for \"sendInlineKeyboard\" Telegram API method" $ do
      evalTelegramT testConfig1 (sendInlineKeyboard_ 111 "some text1" kbd1)
        `shouldReturn` HTTP.POST uri1 json1
      evalTelegramT testConfig2 (sendInlineKeyboard_ 222 "some text2" kbd2)
        `shouldReturn` HTTP.POST uri2 json2
  where
    uri1 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY1/sendMessage"
    kbd1 =
      InlineKeyboardMarkup
        [ [InlineKeyboardButton "abc" "abc1", InlineKeyboardButton "123" "1231"],
          [InlineKeyboardButton "some" "some"]
        ]
    json1 = "{\"text\":\"some text1\",\"chat_id\":111,\"reply_markup\":" <> Aeson.encode kbd1 <> "}"
    uri2 =
      fromJust $
        URI.parseURI "https://api.telegram.org/botKEY2/sendMessage"
    kbd2 =
      InlineKeyboardMarkup
        [ [InlineKeyboardButton "32" "322"],
          [InlineKeyboardButton "abc" "abc1", InlineKeyboardButton "some" "some"]
        ]
    json2 = "{\"text\":\"some text2\",\"chat_id\":222,\"reply_markup\":" <> Aeson.encode kbd2 <> "}"
