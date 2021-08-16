module Bot.Vkontakte where

import qualified API
import qualified API.Vkontakte as VK
import Bot hiding (strings)
import qualified Bot (strings)
import qualified Data.ByteString.Lazy.Char8 as L8

data Config =
    Config
        { key :: String
        , echoMultiplier :: Int
        , strings :: Bot.Strings
        , group_id :: Integer
        , v :: String
        }
    deriving (Show)

new :: Config -> IO Handle
new cfg = undefined

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config io = do
    hBot <- new config
    io hBot

doBotThing :: Handle -> IO L8.ByteString
doBotThing hBot = undefined
