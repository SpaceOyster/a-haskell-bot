{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Vkontakte
    ( module Bot
    , Config(..)
    ) where

import qualified API
import qualified API.Vkontakte as VK
import qualified Bot
import Control.Monad (join, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef (newIORef)
import Data.Maybe (isJust)
import Exceptions (BotException(..))
import qualified Exceptions as Priority (Priority(..))
import Handle.Class (IsHandle(..))
import qualified Logger
import Utils (throwDecode)

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        , group_id :: Integer
        , v :: String
        }
    deriving (Show)

instance IsHandle (Bot.Handle VK.VKState) Config where
    new :: Config -> Logger.Handle -> IO (Bot.Handle VK.VKState)
    new cfg@Config {..} hLog = do
        state <- newIORef $ Bot.BotState {userSettings = mempty}
        hAPI <- VK.new VK.Config {..} hLog
        pure $ Bot.Handle {..}

instance Bot.BotHandle (Bot.Handle VK.VKState) where
    type Update (Bot.Handle VK.VKState) = VK.GroupEvent
    fetchUpdates :: Bot.Handle VK.VKState -> IO [VK.GroupEvent]
    fetchUpdates hBot@Bot.Handle {hAPI, hLog} = do
        Logger.info' hLog "Vkontakte: fetching Updates"
        VK.runMethod hAPI VK.GetUpdates >>= VK.extractUpdates
    logger :: Bot.Handle VK.VKState -> Logger.Handle
    logger = Bot.hLog
    data Entity (Bot.Handle VK.VKState) = EMessage VK.Message
                                    | ECommand VK.Message
                                    | ECallback VK.CallbackEvent
                                    | EOther VK.GroupEvent
                                        deriving (Show)
    type Response (Bot.Handle VK.VKState) = VK.Response
    qualifyUpdate :: VK.GroupEvent -> Bot.Entity (Bot.Handle VK.VKState)
    qualifyUpdate (VK.MessageNew m)
        | isCommandE m = ECommand m
        | otherwise = EMessage m
    qualifyUpdate (VK.MessageEvent c) = ECallback c
    qualifyUpdate _ = EOther VK.Other -- TODO
    reactToUpdate :: Bot.Handle VK.VKState -> VK.GroupEvent -> IO [VK.Response]
    reactToUpdate hBot@Bot.Handle {hLog} update = do
        Logger.info' hLog $ "VK got Update: " <> show update
        let qu = Bot.qualifyUpdate update
        Logger.info' hLog $ "VK qualified Update: " <> show qu
        case qu of
            ECommand msg -> (: []) <$> reactToCommand hBot msg
            EMessage msg -> reactToMessage hBot msg
            ECallback cq -> reactToCallback hBot cq
            EOther _ -> throwM $ Ex Priority.Info "Unknown Update Type."
    type Message (Bot.Handle VK.VKState) = VK.Message
    execCommand ::
           Bot.Handle VK.VKState
        -> Bot.Command
        -> (VK.Message -> IO VK.Response)
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
reactToCommand :: Bot.Handle VK.VKState -> VK.Message -> IO VK.Response
reactToCommand hBot@Bot.Handle {hLog} msg@VK.Message {id, peer_id} = do
    let cmd = getCommand msg
    Logger.debug' hLog $
        "Vkontakte: Got command" <>
        show cmd <>
        " in message id " <> show id <> " , peer_id: " <> show peer_id
    Bot.execCommand hBot cmd msg

-- diff
reactToMessage :: Bot.Handle VK.VKState -> VK.Message -> IO [VK.Response]
reactToMessage hBot@Bot.Handle {hAPI, hLog} msg@VK.Message {..} = do
    n <- Bot.getUserMultiplier hBot $ VK.User from_id
    Logger.debug' hLog $
        "Vkontakte: generating " <> show n <> " echoes for Message: " <> show id
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
reactToCallback :: Bot.Handle VK.VKState -> VK.CallbackEvent -> IO [VK.Response]
reactToCallback hBot cq@VK.CallbackEvent {user_id, event_id, payload} = do
    let callback = A.parseMaybe A.parseJSON payload
    let user = VK.User user_id
    case callback of
        Just (RepeatPayload n) -> do
            Logger.info' (Bot.hLog hBot) $
                "Setting echo multiplier = " <> show n <> " for " <> show user
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
