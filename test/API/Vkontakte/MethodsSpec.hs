{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Vkontakte.MethodsSpec (spec) where

import API.Vkontakte.Methods as VK
  ( copyMessage,
    copyMessage_,
    getUpdates,
    getUpdates_,
    sendKeyboard,
    sendKeyboard_,
    sendMessageEventAnswer,
    sendMessageEventAnswer_,
    sendMessageWith,
    sendTextMessage,
    sendTextMessage_,
  )
import API.Vkontakte.Monad as VK
  ( Config (wait_seconds),
    MonadVkontakte (getVKState),
    VKState (lastTS),
    VkontakteT,
    evalVkontakteT,
    initiatePollServer,
    makeBaseURI,
    makePollURI,
  )
import API.Vkontakte.Types as VK
  ( Message (msg_attachments, msg_peer_id, msg_text),
    Poll (ts, updates),
    PollInitResponse (PollInitServer),
    PollResponse (PollError, PollResponse),
    PollServer (ts),
    Response (ErrorResponse, ResponseWith),
  )
import App.Env as App
  ( Env (Env, envBotReplies, envHTTP, envLogger, envUsersDB),
  )
import App.Monad as App (App, AppEnv, evalApp)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson as A (encode, toJSON)
import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Handlers.HTTP as HTTP (Handle)
import qualified Network.URI.Extended as URI
import Test.App.Error (isAPIError)
import Test.Arbitrary.Text (AnyText (AnyText))
import Test.Arbitrary.Vkontakte.Types ()
import qualified Test.Handlers.HTTP as HTTP
import qualified Test.Handlers.Logger as Logger
import qualified Test.Handlers.UsersDB as DB
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldBe,
    shouldNotBe,
    shouldReturn,
    shouldThrow,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    NonEmptyList (NonEmpty),
    Positive (getPositive),
    generate,
  )

spec :: Spec
spec = do
  copyMessageSpec
  copyMessageSpec_
  getUpdatesSpec
  getUpdatesSpec_
  sendKeyboardSpec
  sendKeyboardSpec_
  sendMessageEventAnswerSpec
  sendMessageEventAnswerSpec_
  sendMessageWithSpec
  sendTextMessageSpec
  sendTextMessageSpec_

instance Log.MonadLog IO where
  doLog _ _ = pure ()

emptyReplies :: BR.Replies
emptyReplies = BR.Replies "" "" "" "" ""

httpTestEnv :: (MonadIO m) => HTTP.Handle -> m App.AppEnv
httpTestEnv http = do
  db <- DB.new
  pure
    Env
      { envLogger = Logger.new,
        envHTTP = http,
        envUsersDB = db,
        envBotReplies = emptyReplies
      }

modelHTTPReply :: (MonadIO m) => Config -> L8.ByteString -> VkontakteT App a -> m a
modelHTTPReply apiCfg replyBS action = do
  cfg' <- HTTP.defaultConfig
  let procFuncs = const . pure <$> [replyBS]
  let cfg =
        cfg'
          { HTTP.processingFunctions = procFuncs,
            HTTP.cycleMode = HTTP.CycleFrom 1
          }
  HTTP.withHandle cfg $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalVkontakteT apiCfg action

modelPollServer :: (MonadIO m) => Config -> VK.PollServer -> VkontakteT App a -> m a
modelPollServer apiCfg pollServer action =
  HTTP.modelHTTPReply (encode (PollInitServer pollServer)) $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalVkontakteT apiCfg action

getUpdatesSpec :: Spec
getUpdatesSpec = describe "getUpdates" $ do
  prop "fetches [GroupEvent] from PollServer" $
    \(vkCfg, poll) -> do
      let reply = A.encode $ VK.PollResponse poll
      modelHTTPReply vkCfg reply getUpdates
        `shouldReturn` VK.updates poll
  prop "updates VKState{lastTS} record" $
    \(vkCfg, poll) -> do
      let reply = A.encode $ VK.PollResponse poll
      modelHTTPReply vkCfg reply (getUpdates >> (lastTS <$> getVKState))
        `shouldReturn` ts (poll :: VK.Poll)
  context "when Poll server responds with Error" $
    prop "throws apiError" $ \(vkCfg, err) -> do
      let reply = A.encode $ VK.PollError err
      modelHTTPReply vkCfg reply getUpdates
        `shouldThrow` isAPIError

sendMessageEventAnswerSpec :: Spec
sendMessageEventAnswerSpec = describe "sendMessageEventAnswer" $ do
  context "when successfully responded to CallbackEvent" $
    prop "returns 1" $ \(vkCfg, cbEvent, AnyText prompt) -> do
      let reply = A.encode $ VK.ResponseWith $ A.toJSON (1 :: Integer)
      modelHTTPReply vkCfg reply (sendMessageEventAnswer cbEvent prompt)
        `shouldReturn` (1 :: Integer)
  context "when API responded with Error" $
    prop "throws apiError" $
      \(vkCfg, cbEvent, AnyText prompt, err) -> do
        let reply = A.encode $ VK.ErrorResponse err
        modelHTTPReply vkCfg reply (sendMessageEventAnswer cbEvent prompt)
          `shouldThrow` isAPIError

sendTextMessageSpec :: Spec
sendTextMessageSpec = describe "sendTextMessage" $ do
  context "when successfully sent text message" $
    prop "returns (message id :: Integer)" $
      \(vkCfg, AnyText msg, peerId) -> do
        msgId <- getPositive <$> generate arbitrary
        let reply = A.encode $ VK.ResponseWith $ A.toJSON msgId
        modelHTTPReply vkCfg reply (sendTextMessage peerId msg)
          `shouldReturn` msgId
  context "when API responded with Error" $
    prop "throws apiError" $
      \(vkCfg, AnyText msg, peerId, err) -> do
        let reply = A.encode $ VK.ErrorResponse err
        modelHTTPReply vkCfg reply (sendTextMessage peerId msg)
          `shouldThrow` isAPIError

copyMessageSpec :: Spec
copyMessageSpec = describe "copyMessage" $ do
  context "when successfully sent message copy" $
    prop "returns (message id :: Integer)" $
      \(vkCfg, vkMsg) -> do
        msgId <- getPositive <$> generate arbitrary
        let reply = A.encode $ VK.ResponseWith $ A.toJSON msgId
        modelHTTPReply vkCfg reply (copyMessage vkMsg)
          `shouldReturn` msgId
  context "when API responded with Error" $
    prop "throws apiError" $
      \(vkCfg, vkMsg, err) -> do
        let reply = A.encode $ VK.ErrorResponse err
        modelHTTPReply vkCfg reply (copyMessage vkMsg)
          `shouldThrow` isAPIError

sendKeyboardSpec :: Spec
sendKeyboardSpec = describe "sendKeyboard" $ do
  context "when successfully sent inline keyboard" $
    prop "returns (message id :: Integer)" $
      \(vkCfg, peerId, AnyText prompt, kbd) -> do
        msgId <- getPositive <$> generate arbitrary
        let reply = A.encode $ VK.ResponseWith $ A.toJSON msgId
        modelHTTPReply vkCfg reply (sendKeyboard peerId prompt kbd)
          `shouldReturn` msgId
  context "when API responded with Error" $
    prop "throws apiError" $
      \(vkCfg, peerId, AnyText prompt, kbd, err) -> do
        let reply = A.encode $ VK.ErrorResponse err
        modelHTTPReply vkCfg reply (sendKeyboard peerId prompt kbd)
          `shouldThrow` isAPIError

getUpdatesSpec_ :: Spec
getUpdatesSpec_ = describe "getUpdates_" $ do
  prop "returns Effects.HTTP.Request for Vkontakte Poll server" $
    \(vkCfg, pollServer) -> do
      let wait = min 90 (max 1 $ wait_seconds (vkCfg :: Config))
      pollURI <- makePollURI pollServer
      let expectedURI =
            URI.addQueryParams
              pollURI
              [ "ts" URI.:=: T.unpack (ts (pollServer :: PollServer)),
                "wait" URI.:=: show wait
              ]
      modelPollServer vkCfg pollServer (VK.initiatePollServer >> getUpdates_)
        `shouldReturn` HTTP.GET expectedURI

sendMessageEventAnswerSpec_ :: Spec
sendMessageEventAnswerSpec_ = describe "sendMessageEventAnswer_" $ do
  prop "returns Effects.HTTP.Request for Vkontakte API server" $
    \(vkCfg, pollServer, cEvent, AnyText prompt) -> do
      let baseURI = makeBaseURI vkCfg
          theMethod = "messages.sendMessageEventAnswer"
          predicate httpReq = case httpReq of
            HTTP.GET uri ->
              (last (URI.pathSegments uri) == theMethod)
                && (URI.uriPath uri /= URI.uriPath baseURI)
            HTTP.POST _ _ -> False
      modelPollServer vkCfg pollServer (predicate <$> sendMessageEventAnswer_ cEvent prompt)
        `shouldReturn` True

sendMessageWithSpec :: Spec
sendMessageWithSpec = describe "sendMessageWith" $ do
  context "helper function for \"messages.send\" API method" $
    context "given peer_id, text and list of QueryParams" $
      prop "builds HTTP.GET request for \"messages.send\" API Method" $
        \(vkCfg, pollServer, peerId, AnyText msgText, qps) -> do
          let baseURI = makeBaseURI vkCfg
              theMethod = "messages.send"
              predicate httpReq = case httpReq of
                HTTP.GET uri ->
                  (last (URI.pathSegments uri) == theMethod)
                    && (URI.uriPath uri /= URI.uriPath baseURI)
                HTTP.POST _ _ -> False
          modelPollServer vkCfg pollServer (predicate <$> sendMessageWith peerId msgText qps)
            `shouldReturn` True

sendTextMessageSpec_ :: Spec
sendTextMessageSpec_ = describe "sendTextMessage_" $ do
  prop "builds HTTP.GET request for sending text Message" $ do
    \(vkCfg, pollServer, peerId, AnyText msgText) -> do
      a <- modelPollServer vkCfg pollServer (sendTextMessage_ peerId msgText)
      b <- modelPollServer vkCfg pollServer (sendMessageWith peerId msgText [])
      a `shouldBe` b

copyMessageSpec_ :: Spec
copyMessageSpec_ = describe "copyMessage_" $ do
  context "when no Attachments present in Message" $
    prop "doesn't differ from sending beck the same text" $
      \(vkCfg, pollServer, vkMsg') -> do
        let vkMsg = vkMsg' {msg_attachments = []}
            peerId = msg_peer_id vkMsg
            msgText = msg_text vkMsg
        a <- modelPollServer vkCfg pollServer (copyMessage_ vkMsg)
        b <- modelPollServer vkCfg pollServer (sendTextMessage_ peerId msgText)
        a `shouldBe` b
  prop "appends attachments to query" $ do
    \(vkCfg, pollServer, vkMsg, NonEmpty attchmts) -> do
      let vkMsgNoAtt = vkMsg {msg_attachments = []}
          vkMsgAtt = vkMsg {msg_attachments = attchmts}
      a <- modelPollServer vkCfg pollServer (copyMessage_ vkMsgAtt)
      b <- modelPollServer vkCfg pollServer (copyMessage_ vkMsgNoAtt)
      a `shouldNotBe` b

sendKeyboardSpec_ :: Spec
sendKeyboardSpec_ = describe "sendKeyboard_" $ do
  prop "does differ from send just text message" $ do
    \(vkCfg, pollServer, peerId, AnyText prompt, kbd) -> do
      a <- modelPollServer vkCfg pollServer (sendKeyboard_ peerId prompt kbd)
      b <- modelPollServer vkCfg pollServer (sendTextMessage_ peerId prompt)
      a `shouldNotBe` b
