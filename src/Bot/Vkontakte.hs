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
    initiate,
  )
import API.Vkontakte.Methods as VK.Methods
import App.Error (botError)
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM_)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (evalStateT, lift, put)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (parseMaybe)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB

data Config = Config
  { key :: String,
    defaultEchoMultiplier :: Int,
    repliesM :: Bot.RepliesM,
    group_id :: Integer,
    v :: String
  }
  deriving (Show)

evalVkontakteT :: (Monad m, MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Config -> VK.VkontakteT m a -> m a
evalVkontakteT cfg t = flip evalStateT mempty $
  VK.unVkontakteT $ do
    st <- initiate cfg
    put st >> t

initiate ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Config -> VK.VkontakteT m VK.VKState
initiate cfg@Config {..} = do
  lift $ Log.logInfo "Initiating Vkontakte Bot"
  lift $ Log.logDebug $ "Vkontakte Bot config: " <> T.tshow cfg
  VK.initiate VK.Config {..}

instance
  ( MonadThrow m,
    Log.MonadLog m,
    HTTP.MonadHTTP m,
    DB.MonadUsersDB m,
    BR.MonadBotReplies m
  ) =>
  Bot.EchoBotMonad (VK.VkontakteT m)
  where
  type Update (VK.VkontakteT m) = VK.GroupEvent
  type Message (VK.VkontakteT m) = VK.Message
  type Command (VK.VkontakteT m) = VK.Message
  type CallbackQuery (VK.VkontakteT m) = VK.CallbackEvent

  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    VK.VkontakteT m [VK.GroupEvent]
  fetchUpdates = do
    lift $ Log.logInfo "Vkontakte: fetching Updates"
    VK.Methods.getUpdates

  qualifyUpdate :: (MonadThrow m) => VK.GroupEvent -> VK.VkontakteT m (Bot.Entity (VK.VkontakteT m))
  qualifyUpdate (VK.MessageNew m)
    | isCommandE m = pure $ Bot.ECommand m
    | otherwise = pure $ Bot.EMessage m
  qualifyUpdate (VK.MessageEvent c) = pure $ Bot.ECallback c

  reactToCallback ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    VK.CallbackEvent ->
    VK.VkontakteT m ()
  reactToCallback cq@VK.CallbackEvent {user_id, payload} = do
    let callback = A.parseMaybe A.parseJSON payload
    let user = VK.User user_id
    case callback of
      Just (RepeatPayload n) -> do
        lift $
          Log.logInfo $
            "setting echo multiplier = " <> T.tshow n <> " for " <> T.tshow user
        prompt <- lift $ BR.getReply Bot.settingsSaved
        lift $ DB.setUserMultiplier user n
        _ <- VK.Methods.sendMessageEventAnswer cq prompt
        pure ()
      Nothing ->
        throwM $ botError $ "Unknown CallbackQuery type: " <> T.tshow payload

  getAuthorsSettings :: VK.Message -> VK.VkontakteT m DB.UserData
  getAuthorsSettings VK.Message {msg_from_id} = do
    let author = VK.User msg_from_id
    lift $ author & DB.getUserData & DB.orDefaultData

  echoMessageNTimes :: VK.Message -> Int -> VK.VkontakteT m ()
  echoMessageNTimes msg n = do
    lift $
      Log.logDebug $
        "generating " <> T.tshow n
          <> " echoes for Message: "
          <> T.tshow (VK.msg_id msg)
    n `replicateM_` VK.Methods.copyMessage msg

  getCommand :: (Monad m) => VK.Message -> VK.VkontakteT m Bot.BotCommand
  getCommand = pure . Bot.parseCommand . T.takeWhile (/= ' ') . T.tail . VK.msg_text

  execCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (VK.Message -> VK.VkontakteT m ())
  execCommand cmd VK.Message {..} = do
    lift $
      Log.logDebug $
        "Got command"
          <> T.tshow cmd
          <> " in message id "
          <> T.tshow msg_id
          <> " , peer_id: "
          <> T.tshow msg_peer_id
    let address = msg_peer_id
    prompt <- lift $ Bot.repeatPrompt $ Just $ VK.User msg_from_id
    replies <- lift BR.getReplies
    _ <- case cmd of
      Bot.Start -> VK.Methods.sendTextMessage address (Bot.greeting replies)
      Bot.Help -> VK.Methods.sendTextMessage address (Bot.help replies)
      Bot.Repeat -> VK.Methods.sendKeyboard address prompt repeatKeyboard
      Bot.UnknownCommand -> VK.Methods.sendTextMessage address (Bot.unknown replies)
    pure ()

newtype Payload
  = RepeatPayload Int

instance A.ToJSON Payload where
  toJSON (RepeatPayload i) = A.object ["repeat" A..= i]

instance A.FromJSON Payload where
  parseJSON =
    A.withObject "FromJSON Bot.Vkontakte.Payload" $ \o ->
      RepeatPayload <$> o A..: "repeat"

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
          action_payload = Just $ A.toJSON $ RepeatPayload i,
          action_link = Nothing
        }

isCommandE :: VK.Message -> Bool
isCommandE VK.Message {msg_text} = Bot.isCommand msg_text
