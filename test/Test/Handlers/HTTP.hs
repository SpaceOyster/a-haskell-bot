module Test.Handlers.HTTP where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Handlers.HTTP as HTTP

new :: L8.ByteString -> HTTP.Handle
new bs = HTTP.Handle {HTTP.sendRequest = pure . const bs}
