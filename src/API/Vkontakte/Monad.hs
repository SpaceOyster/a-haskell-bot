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
import App.Error (AppError, apiError)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify', put)
import Control.Monad.Trans (MonadTrans)
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

class (Monad m) => MonadVkontakte m where
  getVKState :: m VKState
  modifyVKState :: (VKState -> VKState) -> m ()
  modifyVKState f = getVKState >>= putVKState . f
  putVKState :: VKState -> m ()
  putVKState = modifyVKState . const

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

instance (Monad m) => MonadVkontakte (VkontakteT m) where
  getVKState = get
  putVKState = put
  modifyVKState = modify'

data Config = Config
  { key :: String,
    group_id :: Integer,
    v :: String,
    wait_seconds :: Int
  }
  deriving (Show)

emptyVKState :: VKState
emptyVKState =
  VKState
    { lastTS = mempty,
      pollURI = URI.nullURI,
      apiURI = URI.nullURI,
      wait = 25
    }

rememberLastUpdate ::
  (MonadThrow m, Log.MonadLog m, MonadVkontakte m) =>
  PollResponse ->
  m PollResponse
rememberLastUpdate res = modifyVKState (updateStateWith res) >> pure res

updateStateWith :: PollResponse -> (VKState -> VKState)
updateStateWith (PollResponse poll) = \s -> s {lastTS = ts (poll :: Poll)}
updateStateWith _ = id

evalVkontakteT ::
  (Monad m, MonadCatch m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Config ->
  VkontakteT m a ->
  m a
evalVkontakteT cfg t = flip evalStateT emptyVKState $
  unVkontakteT $ do
    st <- initiate cfg
    put st >> t

initiate ::
  (MonadCatch m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, MonadVkontakte m) =>
  Config ->
  m VKState
initiate cfg = do
  Log.logInfo "Initiating Vkontakte Bot"
  Log.logDebug $ "Vkontakte Bot config: " <> T.tshow cfg
  initiate_ cfg `catch` \e -> do
    Log.logError "Failed to initiate Vkontakte Long Poll API"
    throwM (e :: AppError)

initiate_ ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, MonadVkontakte m) =>
  Config ->
  m VKState
initiate_ cfg = do
  Log.logInfo "Initiating Vkontakte API handle"
  let apiURI = makeBaseURI cfg
  let wait = min 90 (max 1 $ wait_seconds (cfg :: Config))
  putVKState $ emptyVKState {apiURI, wait}
  initiatePollServer

apiMethod :: (MonadVkontakte m) => T.Text -> [URI.QueryParam] -> m URI.URI
apiMethod method qps = do
  st <- getVKState
  pure $ flip URI.addQueryParams qps . URI.addPath (apiURI st) $ T.unpack method

vkAPIURI :: URI.URI
vkAPIURI =
  URI.URI
    { URI.uriScheme = "https:",
      URI.uriAuthority =
        Just $
          URI.URIAuth
            { URI.uriUserInfo = "",
              URI.uriRegName = "api.vk.com",
              URI.uriPort = ""
            },
      URI.uriPath = "/method/",
      URI.uriQuery = "",
      URI.uriFragment = ""
    }

makeBaseURI :: Config -> URI.URI
makeBaseURI Config {..} =
  URI.addQueryParams
    vkAPIURI
    [ "v" URI.:=: v,
      "access_token" URI.:=: key,
      "group_id" URI.:=: show group_id
    ]

initiatePollServer :: (MonadThrow m, HTTP.MonadHTTP m, MonadVkontakte m) => m VKState
initiatePollServer = do
  st <- getVKState
  ps@PollServer {ts} <- getLongPollServer
  pollURI <- makePollURI ps
  let pollCreds = st {lastTS = ts, pollURI}
  pure pollCreds

getLongPollServer :: (MonadThrow m, HTTP.MonadHTTP m, MonadVkontakte m) => m PollServer
getLongPollServer = do
  req <- HTTP.GET <$> apiMethod "groups.getLongPollServer" mempty
  json <- HTTP.sendRequest req
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
    T.unpack server
      <> "?act=a_check&key="
      <> T.unpack key
  where
    ex = throwM $ apiError "Unable to parse Vkontakte long poll URL"
