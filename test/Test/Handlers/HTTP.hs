{-# LANGUAGE RecordWildCards #-}

module Test.Handlers.HTTP where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Sequence as Seq (Seq, fromList, index, length)
import qualified Handlers.HTTP as HTTP

type RequestProcessorIO = HTTP.Request -> IO L8.ByteString

type RequestProcessor = HTTP.Request -> L8.ByteString

data Config = Config
  { hitCounter :: IORef Int,
    processingFunctions :: [HTTP.Request -> L8.ByteString],
    cycleInBounds :: CycleMode
  }

defaultConfig :: MonadIO m => m Config
defaultConfig = do
  hitCounterRef <- liftIO $ newIORef (0 :: Int)
  pure
    Config
      { hitCounter = hitCounterRef,
        processingFunctions = [const mempty],
        cycleInBounds = CycleAll
      }

data CycleMode = CycleFrom Int | CycleFromTo Int Int | CycleAll

toBounds :: Seq.Seq a -> CycleMode -> (Int, Int)
toBounds seq cMode = case cMode of
  CycleFrom a -> (max a 0, Seq.length seq)
  CycleFromTo a b -> (max a 0, min b (Seq.length seq))
  CycleAll -> (0, Seq.length seq)

new :: (MonadIO m) => Config -> m HTTP.Handle
new Config {..} = do
  nextReplyIndexRef <- liftIO $ newIORef (0 :: Int)
  let procs = Seq.fromList processingFunctions
      (a, b) = toBounds procs cycleInBounds
      modifyIndex i = if (i + 1) >= b then a else i + 1
      sendRequest = \req -> do
        replyIndex <- readIORef nextReplyIndexRef
        let processFun = Seq.index procs replyIndex
        modifyIORef' nextReplyIndexRef modifyIndex
        modifyIORef' hitCounter (+ 1)
        pure $ processFun req
  pure $ HTTP.Handle {HTTP.sendRequest = sendRequest}

newWithQueue :: (MonadIO m) => [HTTP.Request -> L8.ByteString] -> m HTTP.Handle
newWithQueue fs = do
  cfg <- defaultConfig
  new cfg {processingFunctions = fs}

modelHTTPReply :: (MonadIO m) => L8.ByteString -> (HTTP.Handle -> m a) -> m a
modelHTTPReply replyBS run = do
  http <- newWithQueue [const replyBS]
  run http

modelHTTPReplies :: (MonadIO m) => [L8.ByteString] -> (HTTP.Handle -> m a) -> m a
modelHTTPReplies repliesBS run = do
  http <- newWithQueue $ const <$> repliesBS
  run http

modelHTTPReplyFunc ::
  (MonadIO m) =>
  (HTTP.Request -> L8.ByteString) ->
  (HTTP.Handle -> m a) ->
  m a
modelHTTPReplyFunc fun run = do
  http <- newWithQueue [fun]
  run http

modelHTTPRepliesWithCounter :: (MonadIO m) => [L8.ByteString] -> (HTTP.Handle -> m a) -> m Int
modelHTTPRepliesWithCounter repliesBS run = do
  hitCounterRef <- liftIO $ newIORef (0 :: Int)
  let cfg =
        Config
          { hitCounter = hitCounterRef,
            processingFunctions = const <$> repliesBS,
            cycleInBounds = CycleAll
          }
  http <- new cfg
  _ <- run http
  liftIO $ readIORef hitCounterRef
