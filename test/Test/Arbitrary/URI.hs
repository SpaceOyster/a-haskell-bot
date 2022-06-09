{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary.URI where

import Network.URI (URI (..), URIAuth (..))
import Test.Arbitrary.String (CleanString (getCleanString))
import Test.QuickCheck (Arbitrary (arbitrary), chooseInteger)

instance Arbitrary URI where
  arbitrary = do
    uriScheme <- (<> ":") . getCleanString <$> arbitrary
    uriAuthority <- arbitrary
    uriPath <- ('/' :) . getCleanString <$> arbitrary
    uriQuery <- ('?' :) . getCleanString <$> arbitrary
    uriFragment <- ('#' :) . getCleanString <$> arbitrary
    pure $
      URI
        { uriScheme,
          uriAuthority,
          uriPath,
          uriQuery,
          uriFragment
        }

instance Arbitrary URIAuth where
  arbitrary = do
    uriUserInfo <- ("//" <>) . (<> "@") . getCleanString <$> arbitrary
    uriRegName <- getCleanString <$> arbitrary
    uriPort <- (':' :) . show <$> chooseInteger (0, 65535)
    pure $ URIAuth {uriUserInfo, uriRegName, uriPort}
