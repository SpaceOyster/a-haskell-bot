module Data.List.Extended
    ( module Data.List
    , changeSubseq
    ) where

import Data.List

changeSubseq :: (Eq a) => [a] -> [a] -> [a] -> [a]
changeSubseq l [] _ = l
changeSubseq l sub new = ping l
  where
    subHead = head sub
    ping l =
        case break (== subHead) l of
            (ls, []) -> ls
            (ls, rs) -> pong ls rs
    pong ls rs =
        case stripPrefix sub rs of
            Just rs' -> ls ++ new ++ ping rs'
            Nothing -> ls ++ head rs : ping (tail rs)

changeSubseq' :: (Eq a) => [a] -> [a] -> [a] -> [a]
changeSubseq' l [] _ = l
changeSubseq' l sub new = helper $ break (== head sub) l
  where
    helper (ls, []) = ls
    helper (ls, rs) =
        case stripPrefix sub rs of
            Just rs' -> ls ++ new ++ changeSubseq rs' sub new
            Nothing -> ls ++ head rs : changeSubseq (tail rs) sub new
