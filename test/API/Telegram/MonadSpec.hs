{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Telegram.MonadSpec (spec) where

import API.Telegram.Monad
  ( Config (Config, key),
    MonadTelegram (getTGState),
    TGState (TGState, lastUpdate),
    apiMethod,
    defaultTGState,
    evalTelegramT,
    initiate,
    makeBaseURI,
    newStateFromM,
    rememberLastUpdate,
  )
import API.Telegram.Types (Update (Update))
import Control.Monad.Catch (MonadCatch (..))
import Data.Maybe (fromJust)
import qualified Effects.Log as Log
import Network.URI (parseURI)
import Test.Hspec
  ( Spec,
    anyException,
    context,
    describe,
    it,
    shouldBe,
    shouldNotBe,
    shouldThrow,
  )

spec :: Spec
spec = describe "API.Telegram.Monad" $ do
  newStateFromMSpec
  rememberLastUpdateSpec
  evalTelegramTSpec
  initiateSpec
  makeBaseURISpec
  apiMethodSpec

testUList1 :: [Update]
testUList1 = [Update 123 Nothing Nothing]

testUList2 :: [Update]
testUList2 = [Update 123 Nothing Nothing, Update 321 Nothing Nothing]

testConfig1 :: Config
testConfig1 = Config "" 0

testConfig2 :: Config
testConfig2 = Config "KEY" 100

instance Log.MonadLog Maybe where
  doLog _ _ = pure ()

instance MonadCatch Maybe where
  catch x _h = x

instance Log.MonadLog IO where
  doLog _ _ = pure ()

newStateFromMSpec :: Spec
newStateFromMSpec = describe "newStateFromM" $ do
  context "given nonempty [Update]" $
    it "returns (Just newTGState)" $ do
      newStateFromM [Update 123 Nothing Nothing] defaultTGState
        `shouldBe` Just (defaultTGState {lastUpdate = 124})
      newStateFromM [Update 123 Nothing Nothing, Update 321 Nothing Nothing] defaultTGState
        `shouldBe` Just (defaultTGState {lastUpdate = 322})
  context "given empty [Update]" $
    it "returns Nothing" $ do
      newStateFromM [] defaultTGState `shouldBe` Nothing

rememberLastUpdateSpec :: Spec
rememberLastUpdateSpec = describe "rememberLastUpdate" $ do
  it "returns unchanged [Update] list" $ do
    evalTelegramT testConfig1 (rememberLastUpdate []) `shouldBe` Just []
    evalTelegramT testConfig1 (rememberLastUpdate testUList1)
      `shouldBe` Just testUList1
    evalTelegramT testConfig1 (rememberLastUpdate testUList2)
      `shouldBe` Just testUList2
  context "given empty [Update]" $ it "doesn't change TGState" $ do
    evalTelegramT testConfig1 (rememberLastUpdate [] >> getTGState)
      `shouldBe` (evalTelegramT testConfig1 getTGState :: Maybe TGState)
    evalTelegramT testConfig1 (rememberLastUpdate testUList1 >> getTGState)
      `shouldNotBe` (evalTelegramT testConfig1 getTGState :: Maybe TGState)
  context "given non empty [Update]" $ it "sets TGState {lastUpdate} to last Update id + 1" $ do
    evalTelegramT testConfig1 (rememberLastUpdate testUList1 >> lastUpdate <$> getTGState)
      `shouldBe` Just (124 :: Integer)
    evalTelegramT testConfig1 (rememberLastUpdate testUList2 >> lastUpdate <$> getTGState)
      `shouldBe` Just (322 :: Integer)

evalTelegramTSpec :: Spec
evalTelegramTSpec = describe "evalTelegramT" $ do
  it "initiates TGState with config" $ do
    evalTelegramT testConfig1 getTGState
      `shouldBe` (initiate testConfig1 :: Maybe TGState)
    evalTelegramT testConfig2 getTGState
      `shouldBe` (initiate testConfig2 :: Maybe TGState)
  context "after state initiation" $ it "runs TelegramT transformer" $ do
    evalTelegramT testConfig1 (pure "some") `shouldBe` Just "some"
    evalTelegramT testConfig2 (pure (1 + 1 :: Integer)) `shouldBe` Just 2

initiateSpec :: Spec
initiateSpec = describe "initiate" $ do
  it "returns TGState initiated with Config" $ do
    let uri1 = fromJust $ parseURI "https://api.telegram.org/bot/"
    initiate testConfig1 `shouldBe` Just (TGState 0 uri1 1)
    let uri2 = fromJust $ parseURI "https://api.telegram.org/botKEY/"
    initiate testConfig2 `shouldBe` Just (TGState 0 uri2 100)
  context "when fails to parse api base URI" $ it "throws error" $ do
    initiate (testConfig1 {key = "unparsable key"}) `shouldThrow` anyException

makeBaseURISpec :: Spec
makeBaseURISpec = describe "makeBaseURI" $ do
  it "returns api base URI, generated with api key from config" $ do
    makeBaseURI testConfig1 `shouldBe` parseURI "https://api.telegram.org/bot/"
    makeBaseURI testConfig2 `shouldBe` parseURI "https://api.telegram.org/botKEY/"
  context "when fails to parse api base URI" $ it "throws error" $ do
    makeBaseURI (testConfig1 {key = "unparsable key"}) `shouldThrow` anyException

apiMethodSpec :: Spec
apiMethodSpec = describe "makeBaseURI" $ do
  it "returns URI for specified api method String" $ do
    evalTelegramT testConfig1 (apiMethod "someMethod")
      `shouldBe` parseURI "https://api.telegram.org/bot/someMethod"
    evalTelegramT testConfig2 (apiMethod "someMethod")
      `shouldBe` parseURI "https://api.telegram.org/botKEY/someMethod"
