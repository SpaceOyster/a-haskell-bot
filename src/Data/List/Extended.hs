module Data.List.Extended
  ( module Data.List
  , replaceSubseq
  ) where

import Data.List

replaceSubseq :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceSubseq l [] _ = l
replaceSubseq l sub new = ping l
  where
    subHead = head sub
    ping l' =
      case break (== subHead) l' of
        (ls, []) -> ls
        (ls, rs) -> ls ++ pong rs
    pong rs =
      case stripPrefix sub rs of
        Just rs' -> new ++ ping rs'
        Nothing -> head rs : ping (tail rs)
