module Data.List.Extended
    ( module Data.List
    , changeSubseq
    ) where

import Data.List

changeSubseq :: (Eq a) => [a] -> [a] -> [a] -> [a]
changeSubseq l sub new = helper $ break (== head sub) l
  where
    helper (ls, []) = ls
    helper (ls, rs) =
        case stripPrefix sub rs of
            Just rs' -> ls ++ new ++ changeSubseq rs' sub new
            Nothing -> ls ++ head rs : changeSubseq (tail rs) sub new
