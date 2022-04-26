{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Bot.Vkontakte
  ( Config (..),
    initiate,
    evalVkontakteT,
    VK.VkontakteT (..),
  )
where

import qualified API.Vkontakte as VK
  ( ButtonColor (..),
    CallbackEvent (..),
    Config (..),
    GroupEvent (..),
    Keyboard (..),
    KeyboardAction (..),
    KeyboardActionType (..),
    KeyboardButton (..),
    Message (..),
    User (..),
    VKState (..),
    VkontakteT (..),
    emptyVKState,
    initiate,
  )
import API.Vkontakte.Methods as VK.Methods
import App.Error (AppError, botError)
import qualified Bot
import Control.Monad (replicateM_)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.State (evalStateT, put)
import qualified Data.Aeson as A (FromJSON (..), ToJSON (..), parseJSON)
import qualified Data.Aeson.Types as A (parseMaybe)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import GHC.Generics (Generic)

data Config = Config
  { key :: String,
    defaultEchoMultiplier :: Int,
    repliesM :: BR.RepliesM,
    group_id :: Integer,
    v :: String,
    wait_seconds :: Int
  }
  deriving (Show)

evalVkontakteT ::
  (Monad m, MonadCatch m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Config ->
  VK.VkontakteT m a ->
  m a
evalVkontakteT cfg t = flip evalStateT VK.emptyVKState $
  VK.unVkontakteT $ do
    st <- initiate cfg
    put st >> t

initiate ::
  (MonadCatch m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  Config ->
  VK.VkontakteT m VK.VKState
initiate cfg@Config {..} = do
  Log.logInfo "Initiating Vkontakte Bot"
  Log.logDebug $ "Vkontakte Bot config: " <> T.tshow cfg
  VK.initiate VK.Config {..} `catch` \e -> do
    Log.logError "Failed to initiate Vkontakte Long Poll API"
    throwM (e :: AppError)

instance
  ( MonadThrow m,
    MonadCatch m,
    Log.MonadLog m,
    HTTP.MonadHTTP m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m
  ) =>
  Bot.EchoBotMonad (VK.VkontakteT m)
  where
  type Message (VK.VkontakteT m) = VK.Message
  type Command (VK.VkontakteT m) = VK.Message
  type CallbackQuery (VK.VkontakteT m) = VK.CallbackEvent

  fetchUpdates ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    VK.VkontakteT m [Bot.Entity (VK.VkontakteT m)]
  fetchUpdates = flip catch logError $ do
    Log.logInfo "Vkontakte: fetching Updates"
    updates <- VK.Methods.getUpdates
    pure $ [x | u <- updates, x <- toBotEntity u]
    where
      logError e = do
        Log.logError $ "Failed to fetch Updates: " <> T.tshow (e :: AppError)
        pure []

  reactToCallback ::
    ( MonadCatch m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    VK.CallbackEvent ->
    VK.VkontakteT m ()
  reactToCallback cq = flip catch logError $ do
    qdata <- qualifyQuery cq
    respondToCallback qdata cq
    where
      logError e = Log.logWarning $ T.tshow (e :: AppError)

  getAuthorsSettings ::
    (DB.MonadUsersDB m) =>
    VK.Message ->
    VK.VkontakteT m DB.UserData
  getAuthorsSettings VK.Message {msg_from_id} = do
    let author = VK.User msg_from_id
    author & DB.getUserData & DB.orDefaultData

  echoMessageNTimes ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    VK.Message ->
    Int ->
    VK.VkontakteT m ()
  echoMessageNTimes msg n = do
    Log.logDebug . mconcat $
      [ "generating ",
        T.tshow n,
        " echoes for Message: ",
        T.tshow (VK.msg_id msg)
      ]
    replicateM_ n (VK.Methods.copyMessage msg) `catch` logError
    where
      logError e =
        Log.logError . T.unlines $
          [ "Failed to reply with echo to message: ",
            T.tshow msg,
            "\tWith Error: ",
            T.tshow (e :: AppError)
          ]

  execCommand ::
    ( MonadCatch m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (VK.Message -> VK.VkontakteT m ())
  execCommand cmd msg@VK.Message {..} = flip catch logError $ do
    Log.logDebug . mconcat $
      [ "Got command ",
        T.tshow cmd,
        " in message id: ",
        T.tshow msg_id,
        " , peer_id: ",
        T.tshow msg_peer_id
      ]
    let address = msg_peer_id
    prompt <- Bot.repeatPrompt $ Just $ VK.User msg_from_id
    replies <- BR.getReplies
    _ <- case cmd of
      Bot.Start -> VK.Methods.sendTextMessage address (BR.greeting replies)
      Bot.Help -> VK.Methods.sendTextMessage address (BR.help replies)
      Bot.Repeat -> VK.Methods.sendKeyboard address prompt repeatKeyboard
      Bot.UnknownCommand -> VK.Methods.sendTextMessage address (BR.unknown replies)
    pure ()
    where
      logError e =
        Log.logError . T.unlines $
          [ "Failed to execute " <> T.tshow cmd <> " from message ",
            T.tshow msg,
            "\t With Error: ",
            T.tshow (e :: AppError)
          ]

getCommand :: VK.Message -> Bot.BotCommand
getCommand = Bot.parseCommand . T.takeWhile (/= ' ') . T.tail . VK.msg_text

toBotEntity ::
  (MonadThrow m) =>
  VK.GroupEvent ->
  m (Bot.Entity (VK.VkontakteT n))
toBotEntity (VK.MessageNew m)
  | isCommandE m = pure $ Bot.ECommand (getCommand m) m
  | otherwise = pure $ Bot.EMessage m
toBotEntity (VK.MessageEvent c) = pure $ Bot.ECallback c

newtype CallbackQueryJSON = CallbackQueryJSON {unCallbackQueryJSON :: T.Text}
  deriving (Generic, A.ToJSON, A.FromJSON)

wrapCallbackQuery :: Bot.QueryData -> CallbackQueryJSON
wrapCallbackQuery = CallbackQueryJSON . Bot.encodeQuery

unwrapCallbackQuery :: CallbackQueryJSON -> Maybe Bot.QueryData
unwrapCallbackQuery = Bot.parseQuery . unCallbackQueryJSON

qualifyQuery :: (MonadThrow m) => VK.CallbackEvent -> m Bot.QueryData
qualifyQuery cq = do
  let cbJSON = A.parseMaybe A.parseJSON $ VK.payload cq
  case cbJSON >>= unwrapCallbackQuery of
    Just qdata -> pure qdata
    Nothing -> throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow cq

respondToCallback ::
  (BR.MonadBotReplies m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
  Bot.QueryData ->
  VK.CallbackEvent ->
  VK.VkontakteT m ()
respondToCallback (Bot.QDRepeat n) c = do
  let user = VK.User $ VK.user_id c
  Log.logInfo $ "setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
  prompt <- BR.getReply BR.settingsSaved
  _ <- VK.Methods.sendMessageEventAnswer c prompt
  DB.setUserMultiplier user n

repeatKeyboard :: VK.Keyboard
repeatKeyboard =
  VK.Keyboard
    { one_time = False,
      inline = True,
      buttons = [repeatButton <$> [1 .. 5]]
    }
  where
    repeatButton i =
      VK.KeyboardButton {color = VK.Primary, action = repeatAction i}
    repeatAction i =
      VK.KeyboardAction
        { action_type = VK.Callback,
          action_label = Just $ T.tshow i,
          action_payload =
            Just . A.toJSON $ wrapCallbackQuery (Bot.QDRepeat i),
          action_link = Nothing
        }

isCommandE :: VK.Message -> Bool
isCommandE VK.Message {msg_text} = Bot.isCommand msg_text
