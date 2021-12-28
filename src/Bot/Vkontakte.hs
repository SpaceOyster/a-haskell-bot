{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Vkontakte
    ( Config(..)
    , new
    ) where

import qualified API.Vkontakte as VK
import App.Monad
import qualified Bot
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (parseMaybe)
import Data.Function ((&))
import Data.IORef (newIORef)
import qualified Data.Text.Extended as T
import Exceptions (BotException(..))
import qualified Exceptions as Priority (Priority(..))
import Handle.Class (IsHandle(..))
import qualified Logger as L

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , stringsM :: Bot.StringsM
        , group_id :: Integer
        , v :: String
        }
    deriving (Show)

instance IsHandle (Bot.Handle VK.Handle) Config where
    new :: Config -> L.Handle -> IO (Bot.Handle VK.Handle)
    new cfg@Config {..} hLog = do
        L.logInfo hLog "Initiating Vkontakte Bot"
        L.logDebug hLog $ "Vkontakte Bot config: " <> T.tshow cfg
        state <- newIORef Bot.BotState {userSettings = mempty}
        hAPI <- VK.new VK.Config {..} hLog
        let strings = Bot.fromStrinsM stringsM
        pure $ Bot.Handle {..}

instance Bot.BotHandle (Bot.Handle VK.Handle) where
    type Update (Bot.Handle VK.Handle) = VK.GroupEvent
    fetchUpdates ::
           (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
        => Bot.Handle VK.Handle
        -> m [VK.GroupEvent]
    fetchUpdates Bot.Handle {hAPI} = do
        envLogInfo "Vkontakte: fetching Updates"
        VK.runMethod hAPI VK.GetUpdates >>= VK.extractUpdates
    data Entity (Bot.Handle VK.Handle) = EMessage VK.Message
                                   | ECommand VK.Message
                                   | ECallback VK.CallbackEvent
                                   | EOther VK.GroupEvent
    type Response (Bot.Handle VK.Handle) = VK.Response
    qualifyUpdate :: VK.GroupEvent -> Bot.Entity (Bot.Handle VK.Handle)
    qualifyUpdate (VK.MessageNew m)
        | isCommandE m = ECommand m
        | otherwise = EMessage m
    qualifyUpdate (VK.MessageEvent c) = ECallback c
    qualifyUpdate _ = EOther VK.Other -- TODO
    reactToUpdate ::
           (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
        => Bot.Handle VK.Handle
        -> VK.GroupEvent
        -> m [VK.Response]
    reactToUpdate hBot update = do
        envLogInfo $ "VK got Update: " <> T.tshow update
        let qu = Bot.qualifyUpdate update
        case qu of
            ECommand msg -> (: []) <$> reactToCommand hBot msg
            EMessage msg -> reactToMessage hBot msg
            ECallback cq -> reactToCallback hBot cq
            EOther _ -> throwM $ Ex Priority.Info "Unknown Update Type."
    type Message (Bot.Handle VK.Handle) = VK.Message
    execCommand ::
           (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
        => Bot.Handle VK.Handle
        -> Bot.Command
        -> (VK.Message -> m VK.Response)
    execCommand hBot@Bot.Handle {..} cmd VK.Message {..} = do
        let address = peer_id
        prompt <- Bot.repeatPrompt hBot $ Just $ VK.User from_id
        VK.runMethod hAPI $
            case cmd of
                Bot.Start -> VK.SendTextMessage address (Bot.greeting strings)
                Bot.Help -> VK.SendTextMessage address (Bot.help strings)
                Bot.Repeat -> VK.SendKeyboard address prompt repeatKeyboard
                Bot.UnknownCommand ->
                    VK.SendTextMessage address (Bot.unknown strings)

-- diff
reactToCommand ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Bot.Handle VK.Handle
    -> VK.Message
    -> m VK.Response
reactToCommand hBot msg@VK.Message {msg_id, peer_id} = do
    let cmd = getCommand msg
    envLogDebug $
        "Got command" <>
        T.tshow cmd <>
        " in message id " <> T.tshow msg_id <> " , peer_id: " <> T.tshow peer_id
    Bot.execCommand hBot cmd msg

-- diff
reactToMessage ::
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Bot.Handle VK.Handle
    -> VK.Message
    -> m [VK.Response]
reactToMessage hBot@Bot.Handle {hAPI} msg@VK.Message {..} = do
    n <- Bot.getUserMultiplier hBot $ VK.User from_id
    envLogDebug $
        "generating " <> T.tshow n <> " echoes for Message: " <> T.tshow msg_id
    n `replicateM` VK.runMethod hAPI (VK.CopyMessage msg)

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
       (MonadIO m, MonadThrow m, MonadReader env m, Has L.Handle env)
    => Bot.Handle VK.Handle
    -> VK.CallbackEvent
    -> m [VK.Response]
reactToCallback hBot cq@VK.CallbackEvent {user_id, payload} = do
    let callback = A.parseMaybe A.parseJSON payload
    let user = VK.User user_id
    case callback of
        Just (RepeatPayload n) -> do
            envLogInfo $
                "setting echo multiplier = " <>
                T.tshow n <> " for " <> T.tshow user
            let prompt = hBot & Bot.strings & Bot.settingsSaved
            Bot.setUserMultiplier hBot user n
            fmap (: []) . VK.runMethod (Bot.hAPI hBot) $
                VK.SendMessageEventAnswer cq prompt
        Nothing ->
            throwM $
            Ex Priority.Info $ "Unknown CallbackQuery type: " <> show payload

-- diff
getCommand :: VK.Message -> Bot.Command
getCommand = Bot.parseCommand . takeWhile (/= ' ') . tail . VK.text

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
            , label = Just $ show i
            , payload = Just $ A.toJSON $ RepeatPayload i
            , link = Nothing
            }

-- diff
isCommandE :: VK.Message -> Bool
isCommandE VK.Message {text} = Bot.isCommand text
