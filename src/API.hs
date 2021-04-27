{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}

module API where

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Function ((&))
import qualified HTTP

data Config =
    Config
        { key :: String
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        }

data Handle m =
    Handle
        { http :: HTTP.Handle m
        , helpMessage :: String
        , greeting :: String
        , repeatPrompt :: String
        }

get :: (Monad m) => Handle m -> String -> m L8.ByteString
get hAPI = hAPI & http & HTTP.get

post :: (Monad m) => Handle m -> String -> L8.ByteString -> m L8.ByteString
post hAPI = hAPI & http & HTTP.post
