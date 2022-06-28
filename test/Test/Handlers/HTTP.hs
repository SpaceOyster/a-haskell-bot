module Test.Handlers.HTTP where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence as Seq (fromList, index, length)
import Data.Text.Encoding as T
import Data.Text.Extended as T
import qualified Handlers.HTTP as HTTP

modelHTTPReply :: (MonadIO m) => L8.ByteString -> (HTTP.Handle -> m a) -> m a
modelHTTPReply replyBS run = do
  http <- newWithQueue [replyBS]
  run http

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

modelHTTPReplies :: (MonadIO m) => [L8.ByteString] -> (HTTP.Handle -> m a) -> m a
modelHTTPReplies repliesBS run = do
  http <- newWithQueue repliesBS
  run http

newWithReplyFunc :: (HTTP.Request -> L8.ByteString) -> HTTP.Handle
newWithReplyFunc fun = HTTP.Handle {HTTP.sendRequest = pure . fun}

modelHTTPReplyFunc :: (HTTP.Request -> L8.ByteString) -> (HTTP.Handle -> m a) -> m a
modelHTTPReplyFunc fun run = do
  let http = newWithReplyFunc fun
  run http

newWithQueueAndCounter :: (MonadIO m) => IORef Int -> [L8.ByteString] -> m HTTP.Handle
newWithQueueAndCounter hitCounterRef bss = do
  let repliesSeq = Seq.fromList bss
      len = Seq.length repliesSeq
  nextReplyIndexRef <- liftIO $ newIORef (0 :: Int)
  let sendRequest = \_ -> do
        replyIndex <- readIORef nextReplyIndexRef
        modifyIORef' nextReplyIndexRef ((`mod` len) . (+ 1))
        modifyIORef' hitCounterRef (+ 1)
        pure (Seq.index repliesSeq replyIndex)
  pure $ HTTP.Handle {HTTP.sendRequest = sendRequest}

modelHTTPRepliesWithCounter :: (MonadIO m) => [L8.ByteString] -> (HTTP.Handle -> m a) -> m Int
modelHTTPRepliesWithCounter repliesBS run = do
  hitCounterRef <- liftIO $ newIORef (0 :: Int)
  http <- newWithQueueAndCounter hitCounterRef repliesBS
  run http
  liftIO $ readIORef hitCounterRef
