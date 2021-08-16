{-# LANGUAGE RecordWildCards #-}
module Bot.Vkontakte where

import qualified API
import qualified API.Vkontakte as VK
import Bot hiding (strings)
import qualified Bot (strings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (newIORef)

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        , group_id :: Integer
        , v :: String
        }
    deriving (Show)

new :: Config -> IO (Handle VK.VKState)
new cfg@Config {..} = do
    let hLog = undefined
    state <- newIORef $ BotState {userSettings = mempty}
    hAPI <- VK.new VK.Config {..}
    pure $ Handle {..}

withHandle :: Config -> (Handle VK.VKState -> IO a) -> IO a
withHandle config io = do
    hBot <- new config
    io hBot

doBotThing :: Handle -> IO L8.ByteString
doBotThing hBot = undefined
