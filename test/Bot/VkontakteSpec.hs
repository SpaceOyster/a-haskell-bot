{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bot.VkontakteSpec (spec) where

import API.Vkontakte.Methods as VK
import API.Vkontakte.Monad as VK
import qualified API.Vkontakte.Types as VK
import App.Env as App
import App.Monad as App (App, AppEnv, evalApp)
import qualified Bot
import Bot.Vkontakte as VK
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson as A
import Data.ByteString.Lazy.Char8 as L8 (ByteString, unpack)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List
import Data.Maybe
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log (MonadLog (..))
import qualified Effects.UsersDB as DB
import qualified Handlers.HTTP as HTTP (Handle)
import Network.URI.Extended as URI
import Test.App.Error
import Test.Arbitrary.BotReplies ()
import Test.Arbitrary.Text
import Test.Arbitrary.Vkontakte.Types ()
import qualified Test.Handlers.HTTP as HTTP
import qualified Test.Handlers.Logger as Logger
import qualified Test.Handlers.UsersDB as DB
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  echoBotInstanceSpec
  getCommandSpec
  isCommandESpec
  qualifyQuerySpec
  repeatKeyboardSpec
  respondToCallbackSpec
  toBotEntitySpec

echoBotInstanceSpec :: Spec
echoBotInstanceSpec = describe "EchoBotMonad instance for Telegram" $ do
  fetchUpdatesSpec
  reactToCallbackSpec
  getAuthorsSettingsSpec
  echoMessageNTimesSpec
  execCommandSpec

emptyReplies :: BR.Replies
emptyReplies = BR.Replies "" "" "" "" ""

responseWith :: ToJSON a => a -> ByteString
responseWith a = A.encode $ VK.ResponseWith $ A.toJSON a

errorResponse :: VK.Error -> ByteString
errorResponse err = A.encode $ VK.PollError err

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

modelApp :: MonadIO m => Config -> VkontakteBot App a -> m a
modelApp apiCfg actn = do
  modelHTTPReply apiCfg mempty actn

modelHTTPReply ::
  (MonadIO m) =>
  Config ->
  L8.ByteString ->
  VkontakteBot App a ->
  m a
modelHTTPReply apiCfg replyBS actn = do
  cfg' <- HTTP.defaultConfig
  pollServer <- liftIO $ generate arbitrary
  let procFuncs = const . pure <$> [encode (VK.PollInitServer pollServer), replyBS]
  let cfg =
        cfg'
          { HTTP.processingFunctions = procFuncs,
            HTTP.cycleMode = HTTP.CycleFrom 1
          }
  HTTP.withHandle cfg $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalVkontakteBot apiCfg actn

modelHTTPHelper ::
  MonadIO m =>
  Config ->
  (HTTP.Request -> IO ByteString) ->
  VkontakteBot App b ->
  IORef Int ->
  BR.Replies ->
  m b
modelHTTPHelper apiCfg fun actn hitCounterRef botReplies = do
  cfg' <- HTTP.defaultConfig
  pollServer <- liftIO $ generate arbitrary
  let procFuncs = [const $ pure (encode $ VK.PollInitServer pollServer), fun]
  let cfg =
        cfg'
          { HTTP.hitCounter = hitCounterRef,
            HTTP.processingFunctions = procFuncs,
            HTTP.cycleMode = HTTP.CycleFrom 1
          }
  HTTP.withHandle cfg $ \http -> do
    env' <- httpTestEnv http
    let env = env' {envBotReplies = botReplies}
    liftIO $ App.evalApp env $ evalVkontakteBot apiCfg actn

modelHTTPFuncWReplies ::
  MonadIO m =>
  Config ->
  (BR.Replies -> HTTP.Request -> IO ByteString) ->
  VkontakteBot App a ->
  m a
modelHTTPFuncWReplies apiCfg fun actn = do
  hitCounterRef <- liftIO $ newIORef (0 :: Int)
  botReplies <- liftIO $ generate arbitrary
  let func = fun botReplies
  modelHTTPHelper apiCfg func actn hitCounterRef botReplies

modelHTTPCountr ::
  MonadIO m =>
  Config ->
  L8.ByteString ->
  VkontakteBot App a ->
  m Int
modelHTTPCountr apiCfg replyBS actn = do
  hitCounterRef <- liftIO $ newIORef (0 :: Int)
  botReplies <- liftIO $ generate arbitrary
  let func = const . pure $ replyBS
  _ <- modelHTTPHelper apiCfg func actn hitCounterRef botReplies
  liftIO $ subtract 1 <$> readIORef hitCounterRef

instance Show (Bot.Entity (VkontakteBot m))

instance Eq (Bot.Entity (VkontakteBot m)) where
  (Bot.ECommand cmda a) == (Bot.ECommand cmdb b) = (cmda == cmdb) && (a == b)
  Bot.EMessage a == Bot.EMessage b = a == b
  Bot.ECallback a == Bot.ECallback b = a == b

instance Log.MonadLog IO where
  doLog _ _ = pure ()

fetchUpdatesSpec :: Spec
fetchUpdatesSpec = describe "fetchUpdates" $ do
  context "requests Vkontakte PollServer for GroupEvent list and converts it to [Bot.Entity]" $
    prop "result list is not longer then recieved GroupEvent list" $
      \(vkCfg, events) -> do
        let reply = responseWith (events :: [VK.GroupEvent])
        let predicate entities = length entities <= length events
        modelHTTPReply vkCfg reply (predicate <$> Bot.fetchUpdates)
          `shouldReturn` True
  prop "catches and logs error, returning empty list" $
    \(vkCfg, err) -> do
      let reply = errorResponse err
      modelHTTPReply vkCfg reply Bot.fetchUpdates `shouldReturn` []

reactToCallbackSpec :: Spec
reactToCallbackSpec = describe "reactToCallback" $ do
  prop "responds to CallbackEvent" $
    \(vkCfg, cEvent', Positive n) -> do
      reply <- responseWith <$> generate (arbitrary :: Gen Int)
      let qdJSON = A.toJSON . QueryDataJSON $ Bot.QDRepeat n
      let cEvent = cEvent' {VK.payload = qdJSON}
      modelHTTPCountr vkCfg reply (Bot.reactToCallback cEvent)
        `shouldReturn` 1
  context "if answering CallbackEvent fails" $
    prop "logs error" $ \(vkCfg, cEvent', err, Positive n) -> do
      let reply = errorResponse err
      let qdJSON = A.toJSON . QueryDataJSON $ Bot.QDRepeat n
      let cEvent = cEvent' {VK.payload = qdJSON}
      modelHTTPReply vkCfg reply (Bot.reactToCallback cEvent)
        `shouldReturn` ()
  context "no matter how successfull communication with API was" $
    prop "updates users data in DB" $ \(vkCfg, cEvent', err, Positive n) -> do
      let reply = errorResponse err
      let qdJSON = A.toJSON . QueryDataJSON $ Bot.QDRepeat n
      let cEvent = cEvent' {VK.payload = qdJSON}
      let author = VK.User $ VK.user_id cEvent
      modelHTTPReply vkCfg reply (Bot.reactToCallback cEvent >> DB.getUserMultiplier author)
        `shouldReturn` n

getAuthorsSettingsSpec :: Spec
getAuthorsSettingsSpec = describe "getAuthorsSettings" $ do
  prop "extracts Author of Message and takes UserData from DB" $
    \(vkCfg, vkMsg', Positive n, authorId) -> do
      let qd = DB.UserData n
      let vkMsg = vkMsg' {VK.msg_from_id = authorId}
      let author = VK.User authorId
      modelApp vkCfg (DB.setUserData author qd >> Bot.getAuthorsSettings vkMsg)
        `shouldReturn` qd
  context "when Author is not present in DB" $
    prop "returns default UserData" $
      \(vkCfg, vkMsg) -> do
        defaultUData <- modelApp vkCfg DB.defaultUserData
        modelApp vkCfg (Bot.getAuthorsSettings vkMsg)
          `shouldReturn` defaultUData

echoMessageNTimesSpec :: Spec
echoMessageNTimesSpec = describe "echoMessageNTimes" $ do
  prop "responds to Message with given number of copies" $
    \(vkCfg, vkMsg, Positive n) -> do
      repl <- responseWith <$> generate (arbitrary :: Gen Int)
      modelHTTPCountr vkCfg repl (Bot.echoMessageNTimes vkMsg n)
        `shouldReturn` n
  context "when error happens" $
    prop "catches and logs error" $ \(vkCfg, vkMsg, Positive n, err) -> do
      let repl = errorResponse err
      modelHTTPReply vkCfg repl (Bot.echoMessageNTimes vkMsg n)
        `shouldReturn` ()

execCommandSpec :: Spec
execCommandSpec = describe "execCommand" $ do
  context "on Bot.Start command" $
    prop "sends text message greeting user" $
      propForWith Bot.Start (checkTxt BR.greeting)
  context "on Bot.Help command" $
    prop "sends text message with help prompt" $
      propForWith Bot.Help (checkTxt BR.help)
  context "on Bot.Repeat command" $ do
    prop "sends message repeat prompt" $
      propForWith Bot.Repeat (checkTxt BR.repeat)
    prop "sends message with inline keyboard" $
      propForWith Bot.Repeat checkKbd
  context "on Bot.UnknownCommand command" $
    prop "sends text message, that command is unknown" $
      propForWith Bot.UnknownCommand (checkTxt BR.unknown)
  where
    getQuery :: HTTP.Request -> String
    getQuery (HTTP.GET uri) = URI.uriQuery uri
    getQuery (HTTP.POST uri _) = URI.uriQuery uri
    toQuery :: String -> String -> String
    toQuery k val = fromMaybe "" $ URI.stringifyQueryPair (k URI.:=: val)
    kbdString :: String
    kbdString = L8.unpack $ A.encode repeatKeyboard
    checkTxt :: (BR.Replies -> T.Text) -> BR.Replies -> HTTP.Request -> Bool
    checkTxt repl repls req = toQuery "message" (T.unpack $ repl repls) `isInfixOf` getQuery req
    checkKbd :: BR.Replies -> HTTP.Request -> Bool
    checkKbd _ req = toQuery "keyboard" kbdString `isInfixOf` getQuery req
    propForWith :: Bot.BotCommand -> (BR.Replies -> HTTP.Request -> Bool) -> (Config, VK.Message) -> Expectation
    propForWith cmd check = \(tgCfg, tgMsg) -> do
      let apiReply = responseWith tgMsg
          apiErr = error "didn't pass"
          httpFunc repls req = pure $ if check repls req then apiReply else apiErr
      modelHTTPFuncWReplies tgCfg httpFunc (Bot.execCommand cmd tgMsg)
        `shouldReturn` ()

getCommandSpec :: Spec
getCommandSpec = describe "getCommand" $ do
  context "when Message contains known BotCommand" $
    prop "returns Bot.BotCommand" $ \vkMsg' -> do
      botCmd <- generate $ chooseEnum (minBound, maxBound)
      let vkMsg = vkMsg' {VK.msg_text = ("/" <>) $ T.toLower $ T.tshow botCmd}
      getCommand vkMsg `shouldBe` botCmd
  context "when Message doesnt contain known BotCommand" $
    prop "returns Bot.UnknownCommand" $ \vkMsg ->
      getCommand vkMsg `shouldBe` Bot.UnknownCommand

toBotEntitySpec :: Spec
toBotEntitySpec = describe "toBotEntity" $ do
  context "when GroupEvent is MessageEvent" $
    prop "qualifies GroupEvent as Bot.ECallback" $ \cEvent ->
      toBotEntity (VK.MessageEvent cEvent) `shouldReturn` Bot.ECallback cEvent
  context "when GroupEvent is MessageNew" $ do
    context "and text begins with backslash" $
      prop "qualifies GroupEvent as Bot.ECommand" $ \vkMsg' -> do
        let withBackSlash = "/" <> VK.msg_text vkMsg'
        let vkMsg = vkMsg' {VK.msg_text = withBackSlash}
        let evnt = VK.MessageNew vkMsg
        let cmd = getCommand vkMsg
        toBotEntity evnt `shouldReturn` Bot.ECommand cmd vkMsg
    context "and text doesn't begin with backslash" $
      prop "qualifies GroupEvent as Bot.EMessage" $ \vkMsg' -> do
        let withoutBackSlash = T.dropWhile (== '/') $ VK.msg_text vkMsg'
        let vkMsg = vkMsg' {VK.msg_text = withoutBackSlash}
        let evnt = VK.MessageNew vkMsg
        toBotEntity evnt `shouldReturn` Bot.EMessage vkMsg

qualifyQuerySpec :: Spec
qualifyQuerySpec = describe "qualifyQuery" $ do
  prop "qualifies CallbackEvent as on of Bot.QueryData" $
    \(Positive n, cEvent') -> do
      let qData = Bot.QDRepeat n
      let cEvent = cEvent' {VK.payload = A.toJSON $ VK.QueryDataJSON qData}
      qualifyQuery cEvent `shouldReturn` qData
  context "when CallbackEvent is not valid" $
    prop "throws BotError" $ \cEvent -> do
      qualifyQuery cEvent `shouldThrow` isBotError

respondToCallbackSpec :: Spec
respondToCallbackSpec = describe "respondToCallback" $ do
  context "on Bot.QDRepeat" $ do
    prop "updates Users echo multiplier" $
      \(vkCfg, n, cEvent) -> do
        let reply = responseWith (1 :: Int)
            qData = Bot.QDRepeat n
            doTest = modelHTTPReply vkCfg reply
            author = VK.User $ VK.user_id cEvent
            getAuthorsMultiplier = DB.getUserMultiplier author
        doTest (respondToCallback qData cEvent >> getAuthorsMultiplier)
          `shouldReturn` n
    prop "sends CallbackEvent answer to API" $
      \(vkCfg, n, cEvent) -> do
        let doTest = modelHTTPFuncWReplies vkCfg (httpFunc vkCfg cEvent)
        let qData = Bot.QDRepeat n
        doTest (respondToCallback qData cEvent) `shouldReturn` 1
  where
    httpFunc vkCfg cEvent reps req = do
      let prompt = BR.settingsSaved reps
      supposedHTTPReq <- modelApp vkCfg (VK.sendMessageEventAnswer_ cEvent prompt)
      let result = if req == supposedHTTPReq then 1 else 0 :: Int
      pure $ responseWith result

repeatKeyboardSpec :: Spec
repeatKeyboardSpec =
  describe "repeatKeyboard" $
    it "constructs InlineKeyboardMarkup with 5 buttons with Bot.QDRepet callback" $ do
      let [btnsLine] = VK.buttons VK.repeatKeyboard
      length btnsLine `shouldBe` 5

isCommandESpec :: Spec
isCommandESpec = describe "isCommandE" $ do
  context "when given Message with valid command" $
    prop "returns True" $
      \tgMsg -> do
        botCmd <- generate $ chooseEnum (minBound, maxBound)
        let cmdToText = T.toLower $ T.tshow (botCmd :: Bot.BotCommand)
        isCommandE (tgMsg {VK.msg_text = "/" <> cmdToText})
          `shouldBe` True
  context "when given Message with whatever text starting with back slash" $
    prop "treats text like Bot.UnknownCommand, and returns True" $
      \(tgMsg, AnyText txt) ->
        isCommandE (tgMsg {VK.msg_text = "/" <> txt})
          `shouldBe` True
  context "when given Message with text without \'/\' in the begining" $
    prop "returns False" $
      \(tgMsg, AnyText txt) -> do
        let withoutBackSlash = T.dropWhile (== '/') txt
        isCommandE (tgMsg {VK.msg_text = withoutBackSlash})
          `shouldBe` False
