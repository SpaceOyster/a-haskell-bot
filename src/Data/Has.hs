{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Has
    ( Has(..)
    ) where

class Has field env where
    obtain :: env -> field
