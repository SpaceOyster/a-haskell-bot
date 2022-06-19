module Test.Handlers.HTTP where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Sequence as Seq (fromList, index, length)
import qualified Handlers.HTTP as HTTP

newWithSingle :: L8.ByteString -> HTTP.Handle
newWithSingle bs = HTTP.Handle {HTTP.sendRequest = pure . const bs}


modelHTTPReply :: (MonadIO m) => L8.ByteString -> (HTTP.Handle -> m a) -> m a
modelHTTPReply replyBS run = do
  let http = newWithSingle replyBS
  run http
