module Bot where

import qualified API (Handle)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Logger

data Handle m =
    Handle
        { hAPI :: API.Handle m
        , hLog :: Logger.Handle
        , state :: IORef BotState
        , strings :: Strings
        , echoMultiplier :: Int
        }

data Strings =
    Strings
        { helpM :: Maybe String
        , greetingM :: Maybe String
        , repeatM :: Maybe String
        , unknownM :: Maybe String
        }
    deriving (Show)

getterStringM :: (Strings -> Maybe String) -> String -> (Strings -> String)
getterStringM get deflt = fromMaybe deflt . get

help :: Strings -> String
help = getterStringM helpM ""

greeting :: Strings -> String
greeting = getterStringM greetingM ""

repeat :: Strings -> String
repeat = getterStringM repeatM ""

unknown :: Strings -> String
unknown = getterStringM unknownM ""

instance Semigroup Strings where
    s0 <> s1 =
        Strings
            { helpM = helpM s0 <|> helpM s1
            , greetingM = greetingM s0 <|> greetingM s1
            , repeatM = repeatM s0 <|> repeatM s1
            , unknownM = unknownM s0 <|> unknownM s1
            }

instance Monoid Strings where
    mempty =
        Strings
            { helpM = mempty
            , greetingM = mempty
            , repeatM = mempty
            , unknownM = mempty
            }

hGetState :: Handle m -> IO BotState
hGetState hBot = do
    Logger.debug' (hLog hBot) "Getting BotState"
    readIORef $ state hBot

hSetState :: Handle m -> (BotState -> BotState) -> IO ()
hSetState hBot f = do
    Logger.debug' (hLog hBot) "Setting BotState"
    Logger.debug' (hLog hBot) "Appying state BotState mutating function"
    state hBot `modifyIORef` f

type Hash = L8.ByteString

newtype BotState =
    BotState
        { userSettings :: Map Hash Int
        }
