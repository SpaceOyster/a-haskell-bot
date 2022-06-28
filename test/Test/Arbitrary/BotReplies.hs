{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary.BotReplies where

import qualified Effects.BotReplies as BR
import Test.Arbitrary.Text (AnyText (getAnyText))
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary BR.Replies where
  arbitrary = do
    help <- getAnyText <$> arbitrary
    greeting <- getAnyText <$> arbitrary
    repeat <- getAnyText <$> arbitrary
    unknown <- getAnyText <$> arbitrary
    settingsSaved <- getAnyText <$> arbitrary
    pure BR.Replies {..}
