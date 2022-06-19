module Test.Handlers.HTTP where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Sequence as Seq (fromList, index, length)
import qualified Handlers.HTTP as HTTP

newWithSingle :: L8.ByteString -> HTTP.Handle
newWithSingle bs = HTTP.Handle {HTTP.sendRequest = pure . const bs}

newWithQueue :: (MonadIO m) => [L8.ByteString] -> m HTTP.Handle
newWithQueue bss = do
  let repliesSeq = Seq.fromList bss
      len = Seq.length repliesSeq
  nextReplyIndexRef <- liftIO $ newIORef (0 :: Int)
  let sendRequest = \_ -> do
        replyIndex <- readIORef nextReplyIndexRef
        modifyIORef' nextReplyIndexRef ((`mod` len) . (+ 1))
        pure (Seq.index repliesSeq replyIndex)
  pure $ HTTP.Handle {HTTP.sendRequest = sendRequest}

modelHTTPReply :: (MonadIO m) => L8.ByteString -> (HTTP.Handle -> m a) -> m a
modelHTTPReply replyBS run = do
  let http = newWithSingle replyBS
  run http

modelHTTPReplies :: (MonadIO m) => [L8.ByteString] -> (HTTP.Handle -> m a) -> m a
modelHTTPReplies repliesBS run = do
  http <- newWithQueue repliesBS
  run http
