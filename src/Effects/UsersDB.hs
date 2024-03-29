{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.UsersDB where

import Control.Monad (join)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Function ((&))
import qualified Data.Hashable as H
import Data.Maybe (fromMaybe)

newtype UserData = UserData
  { getEchoMultiplier :: Int
  }
  deriving (Show, Eq)

class Monad m => MonadUsersDB m where
  defaultUserData :: m UserData
  getUserData :: H.Hashable u => u -> m (Maybe UserData)
  setUserData :: H.Hashable u => u -> UserData -> m ()
  setUserData user udata = modifyUserData user (const udata)
  modifyUserData :: H.Hashable u => u -> (UserData -> UserData) -> m ()
  modifyUserData user morph = do
    defaultData <- defaultUserData
    udata <- getUserData user
    setUserData user $ morph $ fromMaybe defaultData udata

instance
  {-# OVERLAPPABLE #-}
  (MonadUsersDB m, MonadTrans t, Monad (t m)) =>
  MonadUsersDB (t m)
  where
  defaultUserData = lift defaultUserData
  getUserData = lift . getUserData
  setUserData u d = lift $ setUserData u d

getUserDataM :: (H.Hashable u, MonadUsersDB m) => Maybe u -> m (Maybe UserData)
getUserDataM maybeUser = join <$> traverse getUserData maybeUser

getUserMultiplier :: (H.Hashable u, MonadUsersDB m) => u -> m Int
getUserMultiplier user =
  getEchoMultiplier <$> (user & getUserData & orDefaultData)

getUserMultiplierM :: (H.Hashable u, MonadUsersDB m) => Maybe u -> m Int
getUserMultiplierM userMaybe =
  getEchoMultiplier
    <$> maybe defaultUserData (orDefaultData . getUserData) userMaybe

setUserMultiplier :: (H.Hashable u, MonadUsersDB m) => u -> Int -> m ()
setUserMultiplier user multiplier = do
  ud <- getUserData user & orDefaultData
  setUserData user $ ud {getEchoMultiplier = multiplier}

-- |
--   suggested usage:
--   @
--   user & getUserData & orDefaultData
--   @
orDefaultData :: (MonadUsersDB m) => m (Maybe UserData) -> m UserData
orDefaultData liftedUserM = liftedUserM >>= maybe defaultUserData pure
