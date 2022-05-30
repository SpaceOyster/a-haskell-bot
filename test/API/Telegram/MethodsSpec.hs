{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Telegram.MethodsSpec (spec) where

import API.Telegram.Methods
  ( MessageID (MessageID),
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
  ( Chat (Chat),
    InlineKeyboardButton (InlineKeyboardButton),
    InlineKeyboardMarkup (InlineKeyboardMarkup),
    Message (Message, chat, date, from, message_id, text),
    Update (Update),
  )
import App.Env as App
  ( Env (Env, envBotReplies, envHTTP, envLogger, envUsersDB),
  )
import App.Monad as App (App, AppEnv, evalApp)
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad.Reader (MonadIO (..))
import Data.Aeson as Aeson (encode)
import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Data.Maybe (fromJust)
import qualified Effects.BotReplies as BR
import Effects.HTTP as HTTP (Request (POST))
import Effects.Log as Log (MonadLog (..))
import Network.URI as URI (parseURI)
import qualified Test.Handlers.HTTP as HTTP
import qualified Test.Handlers.Logger as Logger
import qualified Test.Handlers.UsersDB as DB
import Test.Hspec
  ( Spec,
    anyException,
    context,
    describe,
    it,
    shouldReturn,
    shouldThrow,
  )

spec :: Spec
spec = describe "API.Telegram.Methods" $ do
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
  it "returns [Updates] on success" $ do
    let result1 = "{\"ok\":true,\"result\":[]}"
        result2 = "{\"ok\":true,\"result\":[{\"update_id\":1},{\"update_id\":123}]}"
    modelHTTPReply testConfig1 result1 getUpdates
      `shouldReturn` ([] :: [Update])
    modelHTTPReply testConfig1 result2 getUpdates
      `shouldReturn` [Update 1 Nothing Nothing, Update 123 Nothing Nothing]
  it "throws on Error" $ do
    let errBS = "{\"error_code\":501,\"description\":\"something bad happened\"}"
    modelHTTPReply testConfig1 errBS getUpdates
      `shouldThrow` anyException
  it "throws on unexpected Response" $ do
    let unexpextedRes = "{\"ok\":true,\"result\":\"whatever\"}"
    modelHTTPReply testConfig1 unexpextedRes getUpdates
      `shouldThrow` anyException

answerCallbackQuerySpec :: Spec
answerCallbackQuerySpec = describe "answerCallbackQuerySpec" $ do
  it "returns True on success" $ do
    let result = "{\"ok\":true,\"result\":true}"
    modelHTTPReply testConfig1 result (answerCallbackQuery "query")
      `shouldReturn` True
  it "throws on Error" $ do
    let errBS = "{\"error_code\":501,\"description\":\"something bad happened\"}"
    modelHTTPReply testConfig1 errBS (answerCallbackQuery "query")
      `shouldThrow` anyException
  it "throws on unexpected Response" $ do
    let unexpextedRes = "{\"ok\":true,\"result\":\"whatever\"}"
    modelHTTPReply testConfig1 unexpextedRes (answerCallbackQuery "query")
      `shouldThrow` anyException

copyMessageSpec :: Spec
copyMessageSpec = describe "copyMessageSpec" $ do
  it "returns JSON with only `message_id :: Integer` field" $ do
    let result = "{\"ok\":true,\"result\":{\"message_id\":123}}"
        testMsg =
          Message
            { message_id = 1,
              from = Nothing,
              chat = Chat 321,
              date = 12,
              text = Just "whatever"
            }
    modelHTTPReply testConfig1 result (copyMessage testMsg)
      `shouldReturn` MessageID 123
  it "throws on Error" $ do
    let errBS = "{\"error_code\":501,\"description\":\"something bad happened\"}"
    modelHTTPReply testConfig1 errBS (answerCallbackQuery "query")
      `shouldThrow` anyException
  it "throws on unexpected Response" $ do
    let unexpextedRes = "{\"ok\":true,\"result\":\"whatever\"}"
    modelHTTPReply testConfig1 unexpextedRes (answerCallbackQuery "query")
      `shouldThrow` anyException

sendMessageSpec :: Spec
sendMessageSpec = describe "sendMessageSpec" $ do
  context "given Chat ID and Text, sends text message" $
    it "returns Message on success" $ do
      let result = "{\"ok\":true,\"result\":{\"chat\":{\"id\":123},\"date\":12,\"message_id\":1,\"text\":\"whatever text\"}}"
          chatID = 123
          text1 = "whatever text1"
          testMsg =
            Message
              { message_id = 1,
                from = Nothing,
                chat = Chat 123,
                date = 12,
                text = Just "whatever text"
              }
      modelHTTPReply testConfig1 result (sendMessage chatID text1)
        `shouldReturn` testMsg
  it "throws on Error" $ do
    let errBS = "{\"error_code\":501,\"description\":\"something bad happened\"}"
    modelHTTPReply testConfig1 errBS (answerCallbackQuery "query")
      `shouldThrow` anyException
  it "throws on unexpected Response" $ do
    let unexpextedRes = "{\"ok\":true,\"result\":\"whatever\"}"
    modelHTTPReply testConfig1 unexpextedRes (answerCallbackQuery "query")
      `shouldThrow` anyException

sendInlineKeyboardSpec :: Spec
sendInlineKeyboardSpec = describe "sendInlineKeyboardSpec" $ do
  context "given Chat ID, Text and KeyboardLayout, sends inline keyboard message" $
    it "returns Message on success" $ do
      modelHTTPReply testConfig1 result1 (sendInlineKeyboard 111 "prompt" kbd1) `shouldReturn` msg1
  it "throws on Error" $ do
    let errBS = "{\"error_code\":501,\"description\":\"something bad happened\"}"
    modelHTTPReply testConfig1 errBS (answerCallbackQuery "query")
      `shouldThrow` anyException
  it "throws on unexpected Response" $ do
    let unexpextedRes = "{\"ok\":true,\"result\":\"whatever\"}"
    modelHTTPReply testConfig1 unexpextedRes (answerCallbackQuery "query")
      `shouldThrow` anyException
  where
    kbd1 =
      InlineKeyboardMarkup
        [ [InlineKeyboardButton "abc" "abc1", InlineKeyboardButton "123" "1231"],
          [InlineKeyboardButton "some" "some"]
        ]
    result1 = "{\"ok\":true,\"result\":{\"message_id\":1,\"date\":12,\"text\":\"some text1\",\"chat\":{\"id\":111},\"reply_markup\":" <> Aeson.encode kbd1 <> "}}"
    msg1 =
      Message
        { message_id = 1,
          from = Nothing,
          chat = Chat 111,
          date = 12,
          text = Just "some text1"
        }

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
