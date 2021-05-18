module Bot.Telegram where

import qualified API
import qualified API.Telegram as TG
import Data.IORef (IORef)

data Handle m state =
    Handle
        { api :: API.Handle m state
        , state :: IORef state
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        , defaultRepeat :: Int
        }
