{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bot.TelegramSpec (spec) where

import API.Telegram.Methods as TG (answerCallbackQuery_)
import API.Telegram.Monad as TG (Config, evalTelegramT)
import API.Telegram.Types as TG
  ( CallbackQuery (cq_id, from, query_data),
    Error,
    InlineKeyboardMarkup (InlineKeyboardMarkup),
    Message (from, text),
    Response (ErrorResponse, ResponseWith),
    Update (..),
  )
import App.Env as App
  ( Env (Env, envBotReplies, envHTTP, envLogger, envUsersDB),
  )
import App.Monad as App (App, AppEnv, evalApp)
import qualified Bot
import Bot.Telegram as TG
  ( TelegramBot,
    evalTelegramBot,
    getCommand,
    isCommandE,
    qualifyQuery,
    repeatKeyboard,
    respondToCallback,
    toBotEntity,
  )
import Control.Monad.Catch (MonadCatch (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson as A
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    decode,
    encode,
    object,
  )
import Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import Data.Char (toLower)
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import Effects.Log as Log (MonadLog (..))
import qualified Effects.UsersDB as DB
import qualified Handlers.HTTP as HTTP (Handle)
import Test.App.Error (isBotError)
import Test.Arbitrary.BotReplies ()
import Test.Arbitrary.Telegram.Types ()
import Test.Arbitrary.Text
  ( AnyText (AnyText),
    NonEmptyText (NonEmptyText),
  )
import qualified Test.Handlers.HTTP as HTTP
import qualified Test.Handlers.Logger as Logger
import qualified Test.Handlers.UsersDB as DB
import Test.Hspec
  ( Expectation,
    Spec,
    context,
    describe,
    it,
    shouldBe,
    shouldReturn,
    shouldThrow,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Positive (Positive),
    chooseEnum,
    generate,
  )

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

modelApp :: MonadIO m => Config -> TelegramBot App a -> m a
modelApp apiCfg = modelHTTPReply apiCfg mempty

modelHTTPReply :: (MonadIO m) => Config -> L8.ByteString -> TelegramBot App a -> m a
modelHTTPReply apiCfg replyBS action =
  HTTP.modelHTTPReply replyBS $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalTelegramBot apiCfg action

modelHTTPFunc :: MonadIO m => Config -> (HTTP.Request -> ByteString) -> TelegramBot App a -> m a
modelHTTPFunc apiCfg fun action = do
  HTTP.modelHTTPReplyFunc fun $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalTelegramBot apiCfg action

modelHTTPFuncWReplies :: MonadIO m => Config -> (BR.Replies -> HTTP.Request -> ByteString) -> TelegramBot App a -> m a
modelHTTPFuncWReplies apiCfg fun action = do
  botReplies <- liftIO $ generate arbitrary
  HTTP.modelHTTPReplyFunc (fun botReplies) $ \http -> do
    env' <- httpTestEnv http
    let env = env' {envBotReplies = botReplies}
    liftIO $ App.evalApp env $ evalTelegramBot apiCfg action

modelHTTPCountr :: MonadIO m => Config -> L8.ByteString -> TelegramBot App a -> m Int
modelHTTPCountr apiCfg replyBS action =
  HTTP.modelHTTPRepliesWithCounter [replyBS] $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalTelegramBot apiCfg action

responseWith :: ToJSON a => a -> ByteString
responseWith a = A.encode $ A.object ["ok" .= True, "result" .= a]

errorResponse :: Error -> ByteString
errorResponse err = A.encode $ TG.ErrorResponse err

instance Show (Bot.Entity (TelegramBot m))

instance Eq (Bot.Entity (TelegramBot m)) where
  (Bot.ECommand cmda a) == (Bot.ECommand cmdb b) = (cmda == cmdb) && (a == b)
  Bot.EMessage a == Bot.EMessage b = a == b
  Bot.ECallback a == Bot.ECallback b = a == b

instance Log.MonadLog IO where
  doLog _ _ = pure ()

fetchUpdatesSpec :: Spec
fetchUpdatesSpec = describe "fetchUpdates" $ do
  context "polls Telegram API for Updates list and converts it to [Bot.Entity]" $
    prop "result list is not longer then recieved Updates list" $
      \(tgCfg, updates) -> do
        let reply = responseWith (updates :: [TG.Update])
        let predicate entities = length entities <= length updates
        modelHTTPReply tgCfg reply (predicate <$> Bot.fetchUpdates)
          `shouldReturn` True
  prop "catches and logs error, returning empty list" $
    \(tgCfg, err) -> do
      let reply = errorResponse err
      modelHTTPReply tgCfg reply Bot.fetchUpdates
        `shouldReturn` []

reactToCallbackSpec :: Spec
reactToCallbackSpec = describe "reactToCallback" $ do
  prop "responds to CallbackQuery" $
    \(tgCfg, cQuery', Positive n) -> do
      let repl = responseWith True
      let cQuery = cQuery' {TG.query_data = Just $ "repeat_" <> T.tshow (n :: Int)}
      modelHTTPCountr tgCfg repl (Bot.reactToCallback cQuery)
        `shouldReturn` 1
  context "if answering CallbackQuery fails" $
    prop "logs error" $ \(tgCfg, cQuery, err) -> do
      let reply = errorResponse err
      modelHTTPReply tgCfg reply (Bot.reactToCallback cQuery)
        `shouldReturn` ()
  context "no matter how successfull communication with API was" $
    prop "updates users data in DB" $ \(tgCfg, cQuery', Positive n, err) -> do
      let reply = errorResponse err
      let cQuery = cQuery' {TG.query_data = Just $ "repeat_" <> T.tshow n}
      let author = TG.from (cQuery :: TG.CallbackQuery)
      modelHTTPReply tgCfg reply (Bot.reactToCallback cQuery >> DB.getUserMultiplier author)
        `shouldReturn` n

getAuthorsSettingsSpec :: Spec
getAuthorsSettingsSpec = describe "getAuthorsSettings" $ do
  prop "extracts Author of Message and takes UserData from DB" $
    \(tgCfg, tgMsg', Positive n, author) -> do
      let qd = DB.UserData n
      let tgMsg = tgMsg' {TG.from = Just author} :: TG.Message
      modelApp tgCfg (DB.setUserData author qd >> Bot.getAuthorsSettings tgMsg)
        `shouldReturn` qd
  context "when Author is not present in DB" $
    prop "returns default UserData" $
      \(tgCfg, tgMsg) -> do
        defaultUData <- modelApp tgCfg DB.defaultUserData
        modelApp tgCfg (Bot.getAuthorsSettings tgMsg)
          `shouldReturn` defaultUData

echoMessageNTimesSpec :: Spec
echoMessageNTimesSpec = describe "echoMessageNTimes" $ do
  prop "responds to Message with given number of copies" $
    \(tgCfg, tgMsg, Positive n) -> do
      let repl = responseWith tgMsg
      modelHTTPCountr tgCfg repl (Bot.echoMessageNTimes tgMsg n)
        `shouldReturn` n
  context "when error happens" $
    prop "catches and logs error" $ \(tgCfg, tgMsg, Positive n, err) -> do
      let repl = errorResponse err
      modelHTTPReply tgCfg repl (Bot.echoMessageNTimes tgMsg n)
        `shouldReturn` ()

execCommandSpec :: Spec
execCommandSpec = describe "execCommand" $ do
  context "on Bot.Start command" $
    prop "sends text message greeting user" $
      propForWith Bot.Start BR.greeting
  context "on Bot.Help command" $
    prop "sends text message with help prompt" $
      propForWith Bot.Help BR.help
  context "on Bot.Repeat command" $
    prop "sends message with inline keyboard" $
      propForWith Bot.Repeat BR.repeat
  context "on Bot.UnknownCommand command" $
    prop "sends text message, that command is unknown" $
      propForWith Bot.UnknownCommand BR.unknown
  where
    textFromReq :: HTTP.Request -> Maybe T.Text
    textFromReq (HTTP.GET _) = Nothing
    textFromReq (HTTP.POST _uri json) = A.decode json
    propForWith :: Bot.BotCommand -> (BR.Replies -> T.Text) -> (Config, Message, TG.Error) -> Expectation
    propForWith cmd repl = \(tgCfg, tgMsg, err) -> do
      let pred repls = (Just (repl repls) ==)
      let apiReply = responseWith tgMsg
      let apiErr = errorResponse err
      let httpFunc repls req =
            if pred repls $ textFromReq req
              then apiReply
              else apiErr
      modelHTTPFuncWReplies tgCfg httpFunc (Bot.execCommand cmd tgMsg)
        `shouldReturn` ()

getCommandSpec :: Spec
getCommandSpec = describe "getCommand" $ do
  context "when Message contains known BotCommand" $
    prop "returns Bot.BotCommand" $ \tgMsg' -> do
      botCmd <- generate $ chooseEnum (minBound, maxBound)
      let tgMsg = tgMsg' {text = Just $ ("/" <>) $ T.toLower $ T.tshow botCmd} :: TG.Message
      getCommand tgMsg `shouldBe` botCmd
  context "when Message doesnt contain known BotCommand" $
    prop "returns Bot.UnknownCommand" $ \tgMsg -> do
      getCommand tgMsg `shouldBe` Bot.UnknownCommand

toBotEntitySpec :: Spec
toBotEntitySpec = describe "toBotEntity" $ do
  context "when Update has CalbackQuery attached" $
    prop "qualifies Update as Bot.ECallback" $
      \(Positive updId, cQuery') -> do
        let cQuery = cQuery' :: TG.CallbackQuery
        let upd = Update {update_id = updId, message = Nothing, callback_query = Just cQuery}
        TG.toBotEntity upd `shouldReturn` Bot.ECallback cQuery
  context "when Update has no CalbackQuery attached" $ do
    context "and text begins with backslash" $
      prop "qualifies Update as Bot.ECommand" $
        \(Positive updId, NonEmptyText txt, msg') -> do
          let withBackSlash = "/" <> txt
          let msg = (msg' {text = Just withBackSlash} :: TG.Message)
          let upd = Update {update_id = updId, message = Just msg, callback_query = Nothing}
          let cmd = getCommand msg
          TG.toBotEntity upd `shouldReturn` Bot.ECommand cmd msg
    context "and text doesn't begin with backslash" $
      prop "qualifies Update as Bot.EMessage" $
        \(Positive updId, NonEmptyText txt, msg') -> do
          let withoutBackSlash = T.dropWhile (== '/') txt
          let msg = (msg' {text = Just withoutBackSlash} :: TG.Message)
          let upd = Update {update_id = updId, message = Just msg, callback_query = Nothing}
          TG.toBotEntity upd `shouldReturn` Bot.EMessage msg
  context "when Update is not valid" $
    prop "throws BotError" $
      \(Positive updId) -> do
        let upd = Update {update_id = updId, message = Nothing, callback_query = Nothing}
        TG.toBotEntity upd `shouldThrow` isBotError

qualifyQuerySpec :: Spec
qualifyQuerySpec = describe "qualifyQuery" $ do
  prop "qualifies CallbackQuery as on of Bot.QueryData" $
    \(Positive n, cQuery') -> do
      let qData = Bot.QDRepeat n
      let cQuery = cQuery' {query_data = Just $ Bot.encodeQuery qData}
      qualifyQuery cQuery `shouldReturn` qData
  context "when CallbackQuery is not valid" $
    prop "throws BotError" $ \cQuery -> do
      qualifyQuery cQuery `shouldThrow` isBotError

respondToCallbackSpec :: Spec
respondToCallbackSpec = describe "respondToCallback" $
  context "on Bot.QDRepeat" $ do
    prop "updates Users echo multiplier" $
      \(tgCfg, n, cQuery, resp) -> do
        let reply = responseWith (resp :: Bool)
        let qData = Bot.QDRepeat n
        let doTest = modelHTTPReply tgCfg reply
        let getAuthorsMultiplier = DB.getUserMultiplier $ TG.from (cQuery :: CallbackQuery)
        doTest (respondToCallback qData cQuery >> getAuthorsMultiplier)
          `shouldReturn` n
    prop "sends CallbackQuery answer to API" $
      \(tgCfg, n, cQuery) -> do
        let queryId = TG.cq_id cQuery
        supposedHTTPReq <- TG.evalTelegramT tgCfg (TG.answerCallbackQuery_ queryId)
        let httpFunc req = responseWith (req == supposedHTTPReq)
        let doTest = modelHTTPFunc tgCfg httpFunc
        let qData = Bot.QDRepeat n
        doTest (respondToCallback qData cQuery) `shouldReturn` True

repeatKeyboardSpec :: Spec
repeatKeyboardSpec = describe "repeatKeyboard" $
  it "constructs InlineKeyboardMarkup with 5 buttons with Bot.QDRepet callback" $ do
    let TG.InlineKeyboardMarkup [btnsLine] = TG.repeatKeyboard
    length btnsLine `shouldBe` 5

isCommandESpec :: Spec
isCommandESpec = describe "isCommandE" $ do
  context "when given Message with valid command" $
    prop "returns True" $
      \tgMsg -> do
        botCmd <- generate $ chooseEnum (minBound, maxBound)
        let cmdToText = T.toLower $ T.tshow (botCmd :: Bot.BotCommand)
        isCommandE (tgMsg {text = Just $ "/" <> cmdToText})
          `shouldBe` True
  context "when given Message with whatever text starting with back slash" $
    prop "treats text like Bot.UnknownCommand, and returns True" $
      \(tgMsg, AnyText txt) ->
        isCommandE (tgMsg {text = Just $ "/" <> txt})
          `shouldBe` True
  context "when given Message with text without \'/\' in the begining" $
    prop "returns False" $
      \(tgMsg, AnyText txt) -> do
        let withoutBackSlash = T.dropWhile (== '/') txt
        isCommandE (tgMsg {text = Just withoutBackSlash})
          `shouldBe` False
  context "when Message got no text" $
    prop "returns False" $
      \tgMsg -> do
        isCommandE (tgMsg {text = Nothing})
          `shouldBe` False
