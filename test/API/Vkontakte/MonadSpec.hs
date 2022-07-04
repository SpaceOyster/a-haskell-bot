{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Vkontakte.MonadSpec (spec) where

import API.Vkontakte.Monad as VK
  ( Config (group_id, key, v, wait_seconds),
    MonadVkontakte (getVKState),
    VKState (apiURI, lastTS, pollURI, wait),
    VkontakteT,
    apiMethod,
    emptyVKState,
    evalVkontakteT,
    getLongPollServer,
    initiate,
    initiatePollServer,
    makeBaseURI,
    makePollURI,
    rememberLastUpdate,
    updateStateWith,
    vkAPIURI,
  )
import API.Vkontakte.Types as VK
  ( Poll (ts),
    PollInitResponse (PollInitError, PollInitServer),
    PollResponse (PollError, PollResponse),
    PollServer (key, server),
  )
import App.Env as App
  ( Env (Env, envBotReplies, envHTTP, envLogger, envUsersDB),
  )
import App.Monad as App (App, AppEnv, evalApp)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson as A (encode)
import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Data.Text.Extended as T (unpack)
import qualified Effects.BotReplies as BR
import qualified Effects.Log as Log
import qualified Handlers.HTTP as HTTP (Handle)
import Network.URI.Extended as URI
  ( URI (uriQuery),
    addPath,
    addQueryParams,
  )
import Test.App.Error (isAPIError)
import Test.Arbitrary.Text (NonEmptyCleanText (NonEmptyCleanText))
import Test.Arbitrary.URI ()
import Test.Arbitrary.Vkontakte.Types ()
import qualified Test.Handlers.HTTP as HTTP
import qualified Test.Handlers.Logger as Logger
import qualified Test.Handlers.UsersDB as DB
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldBe,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ()

spec :: Spec
spec = do
  rememberLastUpdateSpec
  updateStateWithSpec
  initiateSpec
  apiMethodSpec
  makeBaseURISpec
  initiatePollServerSpec
  getLongPollServerSpec
  makePollURISpec

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
modelHTTPReply apiCfg replyBS action =
  HTTP.modelHTTPReply replyBS $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalVkontakteT apiCfg action

modelPollServer :: (MonadIO m) => Config -> VK.PollServer -> VkontakteT App a -> m a
modelPollServer apiCfg pollServer action =
  HTTP.modelHTTPReply (encode (PollInitServer pollServer)) $ \http -> do
    env <- httpTestEnv http
    liftIO $ App.evalApp env $ evalVkontakteT apiCfg action

rememberLastUpdateSpec :: Spec
rememberLastUpdateSpec = describe "rememberLastUpdate" $ do
  prop "returns unchanged PollResponse" $ \(vkConfig, pollServer, pollRes) -> do
    modelPollServer vkConfig pollServer (rememberLastUpdate pollRes)
      `shouldReturn` pollRes
  context "given non error PollResponse" $
    prop "sets VKState {lastTS} to lastTS from recieved Poll object" $
      \(vkConfig, pollServer, poll) -> do
        modelPollServer vkConfig pollServer (rememberLastUpdate (VK.PollResponse poll) >> lastTS <$> getVKState)
          `shouldReturn` VK.ts (poll :: VK.Poll)

updateStateWithSpec :: Spec
updateStateWithSpec = describe "updateStateWith" $ do
  context "given non error PollResponse" $
    prop "returns VKState{lastTS} record updater function" $ \(poll, vkState) ->
      updateStateWith (PollResponse poll) vkState
        `shouldSatisfy` \vkSt -> lastTS vkSt == ts (poll :: Poll)
  context "given PollError" $
    prop "returns `id`" $ \(err, vkState) ->
      updateStateWith (PollError err) vkState `shouldBe` vkState

initiateSpec :: Spec
initiateSpec = describe "initiate" $
  prop "returns initial VKState" $ \(vkConfig, pollServer) -> do
    modelPollServer vkConfig pollServer (initiate vkConfig)
      `shouldReturn` VK.emptyVKState
        { apiURI = VK.makeBaseURI vkConfig,
          wait = min 90 (max 1 $ wait_seconds (vkConfig :: Config))
        }

apiMethodSpec :: Spec
apiMethodSpec = describe "apiMethod" $
  prop "returns URI for given api method and QueryParams" $
    \(vkConfig, pollServer, NonEmptyCleanText method, qps) -> do
      let baseURI = VK.makeBaseURI vkConfig
          expectedURI =
            flip URI.addQueryParams qps $
              URI.addPath baseURI (T.unpack method)
      modelPollServer vkConfig pollServer (apiMethod method qps)
        `shouldReturn` expectedURI

makeBaseURISpec :: Spec
makeBaseURISpec = describe "makeBaseURI" $
  prop "constructs base URI for Vkontakte API methods access" $
    \cfg -> do
      let queryString =
            "?v="
              <> VK.v cfg
              <> "&access_token="
              <> key (cfg :: VK.Config)
              <> "&group_id="
              <> show (group_id cfg)
      let expected = VK.vkAPIURI {uriQuery = queryString}
      makeBaseURI cfg `shouldBe` expected

initiatePollServerSpec :: Spec
initiatePollServerSpec = describe "initiatePollServer" $ do
  prop "returns new VKState with pollURI record initiated" $
    \(vkCfg, pollServer) -> do
      expectedPollURI <- makePollURI pollServer
      let check st = pure $ pollURI st == expectedPollURI
      (modelPollServer vkCfg pollServer initiatePollServer >>= check) `shouldReturn` True
  prop "updates api state with new VKState" $
    \(vkCfg, pollServer) -> do
      expectedPollURI <- makePollURI pollServer
      let check st = pure $ pollURI st == expectedPollURI
      (modelPollServer vkCfg pollServer (initiatePollServer >> VK.getVKState) >>= check) `shouldReturn` True

getLongPollServerSpec :: Spec
getLongPollServerSpec = describe "getLongPollServer" $ do
  context "when API responded with PollServer ceredentials" $
    prop "returns PollServer credentials object" $
      \(vkCfg, pollServer) -> do
        let reply = A.encode $ PollInitServer pollServer
        modelHTTPReply vkCfg reply getLongPollServer `shouldReturn` pollServer
  context "when API responded with Error" $
    prop "throws apiError" $ \(vkCfg, err) -> do
      let reply = A.encode $ PollInitError err
      modelHTTPReply vkCfg reply getLongPollServer
        `shouldThrow` isAPIError

makePollURISpec :: Spec
makePollURISpec = describe "makePollURI" $ do
  prop "generates URI for Polling from recieved PollServer credentials" $
    \pollServer -> do
      let expectedURIStr =
            mconcat
              [ T.unpack (server pollServer),
                "?act=a_check&key=",
                T.unpack (key (pollServer :: PollServer))
              ]
      fmap show (makePollURI pollServer) `shouldReturn` expectedURIStr
