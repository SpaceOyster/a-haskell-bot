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
    Method (..),
    Response (..),
    User (..),
    VKState (..),
    VkontakteT (..),
    extractUpdates,
    initiate,
    runMethod,
  )
import qualified App.Error as Ex (BotException (..))
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State (evalStateT, lift)
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
evalVkontakteT cfg t = do
  st <- initiate cfg
  evalStateT (VK.unVkontakteT t) st

initiate ::
  (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) => Config -> m VK.VKState
initiate cfg@Config {..} = do
  Log.logInfo "Initiating Vkontakte Bot"
  Log.logDebug $ "Vkontakte Bot config: " <> T.tshow cfg
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
  type Response (VK.VkontakteT m) = VK.Response
  type Message (VK.VkontakteT m) = VK.Message
  type Command (VK.VkontakteT m) = VK.Message
  type CallbackQuery (VK.VkontakteT m) = VK.CallbackEvent

  fetchUpdates ::
    (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m) =>
    VK.VkontakteT m [VK.GroupEvent]
  fetchUpdates = do
    lift $ Log.logInfo "Vkontakte: fetching Updates"
    VK.runMethod VK.GetUpdates >>= VK.extractUpdates

  qualifyUpdate :: (MonadThrow m) => VK.GroupEvent -> VK.VkontakteT m (Bot.Entity (VK.VkontakteT m))
  qualifyUpdate (VK.MessageNew m)
    | isCommandE m = pure $ Bot.ECommand m
    | otherwise = pure $ Bot.EMessage m
  qualifyUpdate (VK.MessageEvent c) = pure $ Bot.ECallback c

  execCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    Bot.BotCommand ->
    (VK.Message -> VK.VkontakteT m VK.Response)
  execCommand cmd VK.Message {..} = do
    let address = peer_id
    prompt <- lift $ Bot.repeatPrompt $ Just $ VK.User from_id
    replies <- lift BR.getReplies
    VK.runMethod $
      case cmd of
        Bot.Start -> VK.SendTextMessage address (Bot.greeting replies)
        Bot.Help -> VK.SendTextMessage address (Bot.help replies)
        Bot.Repeat -> VK.SendKeyboard address prompt repeatKeyboard
        Bot.UnknownCommand -> VK.SendTextMessage address (Bot.unknown replies)

  reactToCommand ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    VK.Message ->
    VK.VkontakteT m [VK.Response]
  reactToCommand msg@VK.Message {msg_id, peer_id} = do
    let cmd = getCommand msg
    lift $
      Log.logDebug $
        "Got command"
          <> T.tshow cmd
          <> " in message id "
          <> T.tshow msg_id
          <> " , peer_id: "
          <> T.tshow peer_id
    pure <$> Bot.execCommand cmd msg

  reactToCallback ::
    ( MonadThrow m,
      Log.MonadLog m,
      HTTP.MonadHTTP m,
      DB.MonadUsersDB m,
      BR.MonadBotReplies m
    ) =>
    VK.CallbackEvent ->
    VK.VkontakteT m [VK.Response]
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
        fmap (: []) . VK.runMethod $ VK.SendMessageEventAnswer cq prompt
      Nothing ->
        throwM $ Ex.Ex $ "Unknown CallbackQuery type: " <> show payload

  getAuthorsSettings :: VK.Message -> VK.VkontakteT m DB.UserData
  getAuthorsSettings VK.Message {from_id} = do
    let author = VK.User from_id
    lift $ author & DB.getUserData & DB.orDefaultData

  echoMessageNTimes :: VK.Message -> Int -> VK.VkontakteT m [VK.Response]
  echoMessageNTimes msg n = do
    lift $ Log.logDebug $ "generating " <> T.tshow n 
      <> " echoes for Message: " <> T.tshow (VK.msg_id msg)
    n `replicateM` VK.runMethod (VK.CopyMessage msg)

newtype Payload
  = RepeatPayload Int

instance A.ToJSON Payload where
  toJSON (RepeatPayload i) = A.object ["repeat" A..= i]

instance A.FromJSON Payload where
  parseJSON =
    A.withObject "FromJSON Bot.Vkontakte.Payload" $ \o ->
      RepeatPayload <$> o A..: "repeat"

getCommand :: VK.Message -> Bot.BotCommand
getCommand = Bot.parseCommand . T.takeWhile (/= ' ') . T.tail . VK.text

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
          label = Just $ T.tshow i,
          payload = Just $ A.toJSON $ RepeatPayload i,
          link = Nothing
        }

isCommandE :: VK.Message -> Bool
isCommandE VK.Message {text} = Bot.isCommand text
