module Data.Text.Extended
    ( module Data.Text
    , tshow
    ) where

import Data.Text

tshow :: Show a => a -> Text
tshow = pack . show
