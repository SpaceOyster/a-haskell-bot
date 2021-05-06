module API where

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import Data.IORef
import qualified HTTP

data Handle m state =
    Handle
        { http :: HTTP.Handle m
        , state :: IORef state
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        , getUpdates :: Request
        }

data Request
    = GET String
    | POST String L8.ByteString
    deriving (Show)

get :: (Monad m) => Handle m state -> String -> m L8.ByteString
get hAPI = hAPI & http & HTTP.get

post ::
       (Monad m) => Handle m state -> String -> L8.ByteString -> m L8.ByteString
post hAPI = hAPI & http & HTTP.post

sendRequest :: (Monad m) => Handle m state -> Request -> m L8.ByteString
sendRequest hAPI req =
    case req of
        GET method -> get hAPI method
        POST method body -> post hAPI method body

hGetState :: Handle m state -> IO state
hGetState = readIORef . state

hSetState :: Handle m state -> (state -> state) -> IO ()
hSetState hAPI f = state hAPI `modifyIORef` f
