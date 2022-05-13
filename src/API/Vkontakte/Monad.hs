{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Vkontakte.Monad where

import API.Vkontakte.Types
  ( Error (Error, error_code, error_msg),
    Poll (ts),
    PollInitResponse (PollInitError, PollInitServer),
    PollResponse (PollResponse),
    PollServer (..),
  )
import App.Error (apiError)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Control.Monad.State (MonadState, StateT, get, modify', put)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Aeson as A
import qualified Data.Text.Extended as T
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Network.URI.Extended as URI

data VKState = VKState
  { lastTS :: T.Text,
    pollURI :: URI.URI,
    apiURI :: URI.URI,
    wait :: Int
  }
  deriving (Show)

newtype VkontakteT m a = VkontakteT {unVkontakteT :: StateT VKState m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadState VKState,
      MonadTrans
    )

instance (Log.MonadLog m) => Log.MonadLog (VkontakteT m) where
  doLog p t = lift . Log.doLog p $ "[Vkontakte] " <> t

data Config = Config
  { key :: String,
    group_id :: Integer,
    v :: String,
    wait_seconds :: Int
  }

emptyVKState :: VKState
emptyVKState =
  VKState
    { lastTS = mempty,
      pollURI = URI.nullURI,
      apiURI = URI.nullURI,
      wait = 25
    }

rememberLastUpdate ::
  (MonadThrow m, Log.MonadLog m) => PollResponse -> VkontakteT m PollResponse
rememberLastUpdate res = modify' (updateStateWith res) >> pure res

updateStateWith :: PollResponse -> (VKState -> VKState)
updateStateWith (PollResponse poll) = \s -> s {lastTS = ts (poll :: Poll)}
updateStateWith _ = id

initiate ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Config ->
  VkontakteT m VKState
initiate cfg = do
  lift $ Log.logInfo "Initiating Vkontakte API handle"
  apiURI <- makeBaseURI cfg
  let wait = min 90 (max 1 $ wait_seconds (cfg :: Config))
  put $ emptyVKState {apiURI, wait}
  initiatePollServer

apiMethod :: (Monad m) => T.Text -> [URI.QueryParam] -> VkontakteT m URI.URI
apiMethod method qps = do
  st <- get
  pure $ flip URI.addQueryParams qps . URI.addPath (apiURI st) $ T.unpack method

makeBaseURI :: MonadThrow m => Config -> m URI.URI
makeBaseURI Config {..} =
  maybe ex pure . URI.parseURI . mconcat $
    [ "https://api.vk.com/method/?v=",
      v,
      "&access_token=",
      key,
      "&group_id=",
      show group_id
    ]
  where
    ex = throwM $ apiError "Unable to parse Vkontakte API URL"

initiatePollServer :: (MonadThrow m, HTTP.MonadHTTP m) => VkontakteT m VKState
initiatePollServer = do
  st <- get
  ps@PollServer {ts} <- getLongPollServer
  pollURI <- makePollURI ps
  let pollCreds = st {lastTS = ts, pollURI}
  pure pollCreds

getLongPollServer :: (MonadThrow m, HTTP.MonadHTTP m) => VkontakteT m PollServer
getLongPollServer = do
  req <- HTTP.GET <$> apiMethod "groups.getLongPollServer" mempty
  json <- lift $ HTTP.sendRequest req
  case A.decode json of
    Just (PollInitServer r) -> pure r
    Just (PollInitError Error {error_code, error_msg}) ->
      throwM . apiError . mconcat $
        [ "Vkontakte poll server responded with error: ",
          T.tshow error_code,
          ": ",
          error_msg
        ]
    Nothing -> throwM $ apiError $ "Unexpected response: " <> T.lazyDecodeUtf8 json

makePollURI :: MonadThrow m => PollServer -> m URI.URI
makePollURI PollServer {key, server} = do
  maybe ex pure . URI.parseURI $
    T.unpack server <> "?act=a_check&key="
      <> T.unpack key
  where
    ex = throwM $ apiError "Unable to parse Vkontakte long poll URL"
