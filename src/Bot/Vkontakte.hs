{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Bot.Vkontakte
  ( VK.Config (..),
    evalVkontakteBot,
    VkontakteBot (..),
    VK.VkontakteT (..),
    getCommand,
    isCommandE,
    qualifyQuery,
    repeatKeyboard,
    respondToCallback,
    toBotEntity,
  )
where

import API.Vkontakte.Methods as VK.Methods
  ( copyMessage,
    getUpdates,
    sendKeyboard,
    sendMessageEventAnswer,
    sendTextMessage,
  )
import qualified API.Vkontakte.Monad as VK
  ( Config (..),
    MonadVkontakte,
    VkontakteT (..),
    evalVkontakteT,
    initiatePollServer,
  )
import qualified API.Vkontakte.Types as VK
  ( ButtonColor (..),
    CallbackEvent (..),
    GroupEvent (..),
    Keyboard (..),
    KeyboardAction (..),
    KeyboardActionType (..),
    KeyboardButton (..),
    Message (..),
    User (..),
  )
import App.Error (AppError, botError)
import qualified Bot
import Control.Monad (replicateM_, void)
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Aeson as A
  ( FromJSON (..),
    ToJSON (..),
    object,
    parseJSON,
    withObject,
    (.:),
    (.=),
  )
import qualified Data.Aeson.Types as A (parseMaybe)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB

newtype VkontakteBot m a = VkontakteBot {unVkontakteBot :: VK.VkontakteT m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      VK.MonadVkontakte,
      MonadTrans
    )

instance (Log.MonadLog m) => Log.MonadLog (VkontakteBot m) where
  doLog p t = lift . Log.doLog p $ "[Vkontakte] " <> t

evalVkontakteBot ::
  (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
  VK.Config ->
  VkontakteBot m a ->
  m a
evalVkontakteBot cfg action = VK.evalVkontakteT cfg . unVkontakteBot $ do
  _ <- VK.initiatePollServer
  action

instance
  (MonadThrow m, MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m, BR.MonadBotReplies m) =>
  Bot.EchoBotMonad (VkontakteBot m)
  where
  type Message (VkontakteBot m) = VK.Message
  type Command (VkontakteBot m) = VK.Message
  type CallbackQuery (VkontakteBot m) = VK.CallbackEvent

  fetchUpdates ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    VkontakteBot m [Bot.Entity (VkontakteBot m)]
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
    VkontakteBot m ()
  reactToCallback cq = flip catch logError $ do
    qdata <- qualifyQuery cq
    _ <- respondToCallback qdata cq
    pure ()
    where
      logError e = Log.logWarning $ T.tshow (e :: AppError)

  getAuthorsSettings ::
    (DB.MonadUsersDB m) =>
    VK.Message ->
    VkontakteBot m DB.UserData
  getAuthorsSettings VK.Message {msg_from_id} = do
    let author = VK.User msg_from_id
    author & DB.getUserData & DB.orDefaultData

  echoMessageNTimes ::
    (MonadCatch m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    VK.Message ->
    Int ->
    VkontakteBot m ()
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
    (VK.Message -> VkontakteBot m ())
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
    void $ case cmd of
      Bot.Start -> VK.Methods.sendTextMessage address (BR.greeting replies)
      Bot.Help -> VK.Methods.sendTextMessage address (BR.help replies)
      Bot.Repeat -> VK.Methods.sendKeyboard address prompt repeatKeyboard
      Bot.UnknownCommand -> VK.Methods.sendTextMessage address (BR.unknown replies)
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
  m (Bot.Entity (VkontakteBot n))
toBotEntity (VK.MessageNew m)
  | isCommandE m = pure $ Bot.ECommand (getCommand m) m
  | otherwise = pure $ Bot.EMessage m
toBotEntity (VK.MessageEvent c) = pure $ Bot.ECallback c

newtype QueryDataJSON = QueryDataJSON {unQueryDataJSON :: Bot.QueryData}

instance A.ToJSON QueryDataJSON where
  toJSON qdJSON = do
    let qd = unQueryDataJSON qdJSON
    case qd of
      Bot.QDRepeat i -> A.object ["repeat" A..= i]

instance A.FromJSON QueryDataJSON where
  parseJSON = A.withObject "Bot.Vkontakte.QueryDataJSON" $ \o ->
    QueryDataJSON . Bot.QDRepeat <$> o A..: "repeat"

qualifyQuery :: (MonadThrow m) => VK.CallbackEvent -> m Bot.QueryData
qualifyQuery cq = do
  let qdJSON = A.parseMaybe A.parseJSON $ VK.payload cq
  case qdJSON of
    Just qdata -> pure $ unQueryDataJSON qdata
    Nothing -> throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow cq

respondToCallback ::
  (VK.MonadVkontakte m, BR.MonadBotReplies m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m) =>
  Bot.QueryData ->
  VK.CallbackEvent ->
  m Integer
respondToCallback (Bot.QDRepeat n) c = do
  let user = VK.User $ VK.user_id c
  Log.logInfo $ "setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
  prompt <- BR.getReply BR.settingsSaved
  DB.setUserMultiplier user n
  VK.Methods.sendMessageEventAnswer c prompt

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
            Just . A.toJSON $ QueryDataJSON (Bot.QDRepeat i),
          action_link = Nothing
        }

isCommandE :: VK.Message -> Bool
isCommandE VK.Message {msg_text} = Bot.isCommand msg_text
