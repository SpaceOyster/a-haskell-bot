{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Telegram.MonadSpec (spec) where

import API.Telegram.Monad
  ( Config (key, timeout_seconds),
    MonadTelegram (getTGState),
    TGState (..),
    TelegramT,
    apiMethod,
    evalTelegramT,
    initiate,
    makeBaseURI,
    newStateFromM,
    rememberLastUpdate,
    tgAPIURI,
  )
import API.Telegram.Types (Update (update_id))
import Control.Monad.Catch (MonadCatch (..))
import qualified Effects.Log as Log
import Network.URI (URI (uriPath))
import Test.Arbitrary.String (CleanString (CleanString))
import Test.Arbitrary.Telegram.Types ()
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldBe,
    shouldReturn,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (NonEmpty))

spec :: Spec
spec = do
  newStateFromMSpec
  rememberLastUpdateSpec
  evalTelegramTSpec
  initiateSpec
  makeBaseURISpec
  apiMethodSpec

instance Log.MonadLog Maybe where
  doLog _ _ = pure ()

instance MonadCatch Maybe where
  catch x _h = x

instance Log.MonadLog IO where
  doLog _ _ = pure ()

newStateFromMSpec :: Spec
newStateFromMSpec = describe "newStateFromM" $ do
  context "given nonempty [Update]" $
    prop "returns (Just newTGState)" $ \(tgState, NonEmpty updates) ->
      newStateFromM updates tgState
        `shouldBe` Just (tgState {lastUpdate = update_id (last updates) + 1})
  context "given empty [Update]" $
    prop "returns Nothing" $ \tgState ->
      newStateFromM [] tgState `shouldBe` Nothing

rememberLastUpdateSpec :: Spec
rememberLastUpdateSpec = describe "rememberLastUpdate" $ do
  prop "returns unchanged [Update] list" $ \(tgConfig, updates) ->
    evalTelegramT tgConfig (rememberLastUpdate updates) `shouldReturn` updates
  context "given empty [Update]" $
    prop "doesn't change TGState" $ \tgConfig ->
      evalTelegramT tgConfig (rememberLastUpdate [] >> getTGState)
        `shouldBe` (evalTelegramT tgConfig getTGState :: Maybe TGState)
  context "given non empty [Update]" $
    prop "sets TGState {lastUpdate} to last Update id + 1" $
      \(tgConfig, NonEmpty updates) ->
        evalTelegramT tgConfig (rememberLastUpdate updates >> lastUpdate <$> getTGState)
          `shouldReturn` update_id (last updates) + 1

evalTelegramTSpec :: Spec
evalTelegramTSpec = describe "evalTelegramT" $ do
  prop "initiates TGState with config" $ \tgConfig ->
    evalTelegramT tgConfig getTGState
      `shouldBe` (initiate tgConfig :: Maybe TGState)
  context "after state initiation" $
    prop "runs TelegramT transformer" $ \tgConfig ->
      evalTelegramT tgConfig (pure "whatever") `shouldBe` Just "whatever"

initiateSpec :: Spec
initiateSpec = describe "initiate" $ do
  prop "returns TGState initiated with Config" $ \tgConfig -> do
    let uri = tgAPIURI {uriPath = uriPath tgAPIURI <> key tgConfig <> "/"}
    let newTimeout = min 100 (max 1 $ timeout_seconds tgConfig)
    let expected = TGState {lastUpdate = 0, apiURI = uri, timeout = newTimeout}
    initiate tgConfig `shouldReturn` expected

makeBaseURISpec :: Spec
makeBaseURISpec = describe "makeBaseURI" $ do
  prop "returns api base URI, generated with api key from config" $
    \tgConfig -> do
      let uri = tgAPIURI {uriPath = uriPath tgAPIURI <> key tgConfig <> "/"}
      makeBaseURI tgConfig `shouldBe` uri

apiMethodSpec :: Spec
apiMethodSpec = describe "makeBaseURI" $ do
  prop "returns URI for specified api method String" $
    \(tgConfig, CleanString method) ->
      evalTelegramT tgConfig (apiMethod method)
        `shouldBe` evalTelegramT tgConfig (testAction method)
  where
    testAction :: String -> TelegramT Maybe URI
    testAction method = do
      st <- getTGState
      let uri = apiURI st
      pure $ uri {uriPath = uriPath uri <> method}
