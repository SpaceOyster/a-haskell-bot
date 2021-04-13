{-# LANGUAGE OverloadedStrings #-}

module API.Telegram where

import API.Telegram.Types

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import HTTP
import Network.HTTP.Client as HTTP (RequestBody(..), responseBody)
import Utils (throwDecode)

getUpdates :: (MonadThrow m) => Handle m -> m [Update]
getUpdates handle = do
    let req = getRequest handle "getUpdates"
    res <- sendRequest handle req
    let json = responseBody res
    res' <- throwDecode json
    getResult res'

echoMessage :: (Monad m) => Handle m -> Message -> m L8.ByteString
echoMessage handle msg = do
    let req =
            postRequest handle "copyMessage" .
            RequestBodyLBS . encode . copyMessage $
            msg
    res <- sendRequest handle req
    return $ responseBody res

withHandle :: (Handle IO -> IO a) -> IO a
withHandle io = do
    config <- parseConfig
    handle <- new config
    io handle

echoAll :: (MonadThrow m) => Handle m -> m [L8.ByteString]
echoAll handle = do
    updates <- getUpdates handle -- add error handling
    mapM (echoMessage handle) $ message <$> updates

reactToUpdate :: (Monad m) => Handle m -> Update -> m L8.ByteString
reactToUpdate handle update = echoMessage handle $ message update

reactToUpdates :: (Monad m) => Handle m -> [Update] -> m [L8.ByteString]
reactToUpdates handle updates = mapM (reactToUpdate handle) updates
