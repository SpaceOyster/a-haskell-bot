{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module Bot.Telegram where

import qualified API
import qualified API.Telegram as TG
import API.Telegram.Types
import Bot
import Data.IORef (IORef)
import qualified Data.Map as Map (Map, alter, findWithDefault)
import qualified Exceptions as Priority (Priority(..))
import Exceptions (BotException(..))

data BotState =
    BotState
        { lastUpdate :: Integer
        , userSettings :: Map.Map User Int
        }

getUserSettings :: Handle m BotState -> User -> IO Int
getUserSettings hBot user = do
    st <- Bot.hGetState hBot
    let drepeats = defaultRepeat hBot
        repeats = Map.findWithDefault drepeats user $ userSettings st
    return repeats

setUserSettings :: Handle m BotState -> User -> Int -> IO ()
setUserSettings hBot user repeats = do
    hBot `Bot.hSetState` \st ->
        let usettings = Map.alter (const $ Just repeats) user $ userSettings st
         in st {userSettings = usettings}

reactToUpdate :: Handle IO BotState -> Update -> IO [API.Request]
reactToUpdate hBot update = do
    let qu = TG.qualifyUpdate update
    case qu of
        TG.ECommand msg -> (: []) <$> reactToCommand hBot msg
        TG.EMessage msg -> reactToMessage hBot msg
        TG.ECallback cq -> (: []) <$> reactToCallback hBot cq
        TG.EOther Update {update_id} ->
            throwM $
            Ex Priority.Info $ "Unknown Update Type. Update: " ++ show update_id
