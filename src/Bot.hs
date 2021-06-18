module Bot where

import qualified API (Handle)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)
import GHC.Generics (Generic)

data Handle m =
    Handle
        { hAPI :: API.Handle m
        , state :: IORef BotState
        , strings :: Strings
        , echoMultiplier :: Int
        }

data Strings =
    Strings
        { helpM :: Maybe String
        , greetingM :: Maybe String
        , repeatM :: Maybe String
        }
    deriving (Show)

getterStringM :: (Strings -> Maybe String) -> String -> (Strings -> String)
getterStringM get deflt =
    \ss ->
        case get ss of
            Just a -> a
            Nothing -> deflt

help :: Strings -> String
help = getterStringM helpM ""

greeting :: Strings -> String
greeting = getterStringM greetingM ""

repeat :: Strings -> String
repeat = getterStringM repeatM ""

instance Semigroup Strings where
    s0 <> s1 =
        Strings
            { helpM = helpM s0 <|> helpM s1
            , greetingM = greetingM s0 <|> greetingM s1
            , repeatM = repeatM s0 <|> repeatM s1
            }

instance Monoid Strings where
    mempty = Strings {helpM = mempty, greetingM = mempty, repeatM = mempty}

hGetState :: Handle m -> IO BotState
hGetState = readIORef . state

hSetState :: Handle m -> (BotState -> BotState) -> IO ()
hSetState hAPI f = state hAPI `modifyIORef` f

type Hash = L8.ByteString

newtype BotState =
    BotState
        { userSettings :: Map Hash Int
        }
