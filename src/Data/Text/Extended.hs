module Data.Text.Extended
  ( module Data.Text,
    tshow,
    lazyDecodeUtf8,
  )
where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text
import qualified Data.Text.Encoding as TE

tshow :: Show a => a -> Text
tshow = pack . show

lazyDecodeUtf8 :: L8.ByteString -> Text
lazyDecodeUtf8 = TE.decodeUtf8 . L8.toStrict
