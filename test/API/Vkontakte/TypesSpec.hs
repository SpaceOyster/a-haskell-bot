module API.Vkontakte.TypesSpec (spec) where

import API.Vkontakte.Types
  ( Poll (Poll),
    PollResponse (PollError, PollResponse),
    Response (ErrorResponse, ResponseWith),
    attachmentToQuery,
    extractUpdates,
    fromResponse,
  )
import Data.Aeson (KeyValue ((.=)), Value, object)
import Network.URI.Extended (QueryParam ((:=:)))
import Test.App.Error as App (isAPIError)
import Test.Arbitrary.Text (AnyText (AnyText))
import Test.Arbitrary.Vkontakte.Types ()
import Test.Hspec
  ( Spec,
    context,
    describe,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
  )
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  attachmentToQuerySpec
  extractUpdatesSpec
  fromResponseSpec

attachmentToQuerySpec :: Spec
attachmentToQuerySpec = describe "attachmentToQuery" $
  prop "turns Attachment to QueryParam" $ \att -> do
    attachmentToQuery att `shouldSatisfy` \(k :=: v) ->
      not (null v) && (k == "attachment" || k == "sticker_id")

extractUpdatesSpec :: Spec
extractUpdatesSpec = describe "attachmentToQuery" $ do
  context "when Poll server responded with updates" $
    prop "returns [GroupEvent]" $ \poll@(Poll ts upds) ->
      extractUpdates (PollResponse poll) `shouldReturn` upds
  context "when Poll server responded with error" $
    prop "throws apiError" $ \err ->
      extractUpdates (PollError err) `shouldThrow` isAPIError

fromResponseSpec :: Spec
fromResponseSpec = describe "attachmentToQuery" $ do
  context "when API server responded with JSON" $
    prop "extracts data of Aeson.ToJSON class from API.Telegram.Response" $
      \kvPairs -> do
        let jsonObject = object $ fmap (\(AnyText k, AnyText v) -> k .= v) kvPairs
        fromResponse (ResponseWith jsonObject) `shouldReturn` jsonObject
  context "when json can't be parsed to expected data type" $
    prop "throws APIError error" $
      \kvPairs -> do
        let jsonObject = object $ fmap (\(AnyText k, AnyText v) -> k .= v) kvPairs
        (fromResponse (ResponseWith jsonObject) :: IO ())
          `shouldThrow` isAPIError
  context "when API server responded with error" $
    prop "throws APIError error" $ \err -> do
      (fromResponse (ErrorResponse err) :: IO Value) `shouldThrow` isAPIError
