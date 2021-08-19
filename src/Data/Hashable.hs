module Data.Hashable
    ( Hashable(..)
    , Hash
    ) where

type Hash = Integer

class Hashable a where
    hash :: a -> Hash
