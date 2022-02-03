{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Bot.Vkontakte
  ( Config(..)
  , new
  ) where

import qualified API.Vkontakte as VK
import qualified Bot
import qualified Bot.Replies as Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State (StateT, lift)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (parseMaybe)
import Data.Function ((&))
import qualified Data.Text.Extended as T
import qualified Effects.BotReplies as BR
import qualified Effects.HTTP as HTTP
import qualified Effects.Log as Log
import qualified Effects.UsersDB as DB
import qualified Exceptions as Ex (BotException(..), Priority(..))

data Config =
  Config
    { key :: String
    , defaultEchoMultiplier :: Int
    , repliesM :: Bot.RepliesM
    , group_id :: Integer
    , v :: String
    }
  deriving (Show)

new ::
     (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m)
  => Config
  -> StateT VK.VKState m (Bot.Handle VK.VKState)
new cfg@Config {..} = do
  lift $ Log.logInfo "Initiating Vkontakte Bot"
  lift $ Log.logDebug $ "Vkontakte Bot config: " <> T.tshow cfg
  VK.new VK.Config {..}
  let replies = Bot.fromRepliesM repliesM
  pure $ Bot.Handle {}

instance Bot.StatefulBotMonad VK.VKState where
  type BotHandle VK.VKState = Bot.Handle VK.VKState
  type Update VK.VKState = VK.GroupEvent
  fetchUpdates ::
       (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m)
    => Bot.Handle VK.VKState
    -> StateT VK.VKState m [VK.GroupEvent]
  fetchUpdates _ = do
    lift $ Log.logInfo "Vkontakte: fetching Updates"
    VK.runMethod VK.GetUpdates >>= VK.extractUpdates
  data Entity VK.VKState = EMessage VK.Message
                       | ECommand VK.Message
                       | ECallback VK.CallbackEvent
  type Response VK.VKState = VK.Response
  qualifyUpdate :: VK.GroupEvent -> Bot.Entity VK.VKState
  qualifyUpdate (VK.MessageNew m)
    | isCommandE m = ECommand m
    | otherwise = EMessage m
  qualifyUpdate (VK.MessageEvent c) = ECallback c
  reactToUpdate ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => Bot.Handle VK.VKState
    -> VK.GroupEvent
    -> StateT VK.VKState m [VK.Response]
  reactToUpdate hBot update = do
    lift $ Log.logInfo $ "VK got Update: " <> T.tshow update
    let qu = Bot.qualifyUpdate update
    case qu of
      ECommand msg -> (: []) <$> reactToCommand hBot msg
      EMessage msg -> reactToMessage hBot msg
      ECallback cq -> reactToCallback hBot cq
  type Message VK.VKState = VK.Message
  execCommand ::
       ( MonadThrow m
       , Log.MonadLog m
       , HTTP.MonadHTTP m
       , DB.MonadUsersDB m
       , BR.MonadBotReplies m
       )
    => Bot.Handle VK.VKState
    -> Bot.Command
    -> (VK.Message -> StateT VK.VKState m VK.Response)
  execCommand hBot cmd VK.Message {..} = do
    let address = peer_id
    prompt <- lift $ Bot.repeatPrompt $ Just $ VK.User from_id
    replies <- lift BR.getReplies
    VK.runMethod $
      case cmd of
        Bot.Start -> VK.SendTextMessage address (Bot.greeting replies)
        Bot.Help -> VK.SendTextMessage address (Bot.help replies)
        Bot.Repeat -> VK.SendKeyboard address prompt repeatKeyboard
        Bot.UnknownCommand -> VK.SendTextMessage address (Bot.unknown replies)

-- diff
reactToCommand ::
     ( MonadThrow m
     , Log.MonadLog m
     , HTTP.MonadHTTP m
     , DB.MonadUsersDB m
     , BR.MonadBotReplies m
     )
  => Bot.Handle VK.VKState
  -> VK.Message
  -> StateT VK.VKState m VK.Response
reactToCommand hBot msg@VK.Message {msg_id, peer_id} = do
  let cmd = getCommand msg
  lift $
    Log.logDebug $
    "Got command" <>
    T.tshow cmd <>
    " in message id " <> T.tshow msg_id <> " , peer_id: " <> T.tshow peer_id
  Bot.execCommand hBot cmd msg

-- diff
reactToMessage ::
     (MonadThrow m, Log.MonadLog m, HTTP.MonadHTTP m, DB.MonadUsersDB m)
  => Bot.Handle VK.VKState
  -> VK.Message
  -> StateT VK.VKState m [VK.Response]
reactToMessage _ msg@VK.Message {..} = do
  n <- lift $ DB.getUserMultiplier $ VK.User from_id
  lift $
    Log.logDebug $
    "generating " <> T.tshow n <> " echoes for Message: " <> T.tshow msg_id
  n `replicateM` VK.runMethod (VK.CopyMessage msg)

newtype Payload =
  RepeatPayload Int

instance A.ToJSON Payload where
  toJSON (RepeatPayload i) = A.object ["repeat" A..= i]

instance A.FromJSON Payload where
  parseJSON =
    A.withObject "FromJSON Bot.Vkontakte.Payload" $ \o ->
      RepeatPayload <$> o A..: "repeat"

-- diff
reactToCallback ::
     ( MonadThrow m
     , Log.MonadLog m
     , HTTP.MonadHTTP m
     , DB.MonadUsersDB m
     , BR.MonadBotReplies m
     )
  => Bot.Handle VK.VKState
  -> VK.CallbackEvent
  -> StateT VK.VKState m [VK.Response]
reactToCallback hBot cq@VK.CallbackEvent {user_id, payload} = do
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
      throwM $ Ex.Ex Ex.Info $ "Unknown CallbackQuery type: " <> show payload

-- diff
getCommand :: VK.Message -> Bot.Command
getCommand = Bot.parseCommand . T.takeWhile (/= ' ') . T.tail . VK.text

-- diff
repeatKeyboard :: VK.Keyboard
repeatKeyboard =
  VK.Keyboard
    {one_time = False, inline = True, buttons = [repeatButton <$> [1 .. 5]]}
  where
    repeatButton i =
      VK.KeyboardButton {color = VK.Primary, action = repeatAction i}
    repeatAction i =
      VK.KeyboardAction
        { action_type = VK.Callback
        , label = Just $ T.tshow i
        , payload = Just $ A.toJSON $ RepeatPayload i
        , link = Nothing
        }

-- diff
isCommandE :: VK.Message -> Bool
isCommandE VK.Message {text} = Bot.isCommand text
