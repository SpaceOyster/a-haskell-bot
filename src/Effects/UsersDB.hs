module Effects.UsersDB where

import Data.Function ((&))
import qualified Data.Hashable as H

newtype UserData =
  UserData
    { getEchoMultiplier :: Int
    }

class Monad m =>
      MonadUsersDB m
  where
  defaultUserData :: m UserData
  getUserData :: H.Hashable u => u -> m (Maybe UserData)
  modifyUserData :: H.Hashable u => u -> (UserData -> UserData) -> m ()

getUserMultiplier :: (H.Hashable u, MonadUsersDB m) => u -> m Int
getUserMultiplier user =
  getEchoMultiplier <$> (user & getUserData & orDefaultData)

getUserMultiplierM :: (H.Hashable u, MonadUsersDB m) => Maybe u -> m Int
getUserMultiplierM userMaybe =
  getEchoMultiplier <$>
  maybe defaultUserData (orDefaultData . getUserData) userMaybe

setUserData :: (H.Hashable u, MonadUsersDB m) => u -> UserData -> m ()
setUserData user d = modifyUserData user $ const d

setUserMultiplier :: (H.Hashable u, MonadUsersDB m) => u -> Int -> m ()
setUserMultiplier user multiplier =
  modifyUserData user $ \us -> us {getEchoMultiplier = multiplier}

{-|
   suggested usage: 
   @
   user & getUserData & orDefaultData
   @
-}
orDefaultData :: (MonadUsersDB m) => m (Maybe UserData) -> m UserData
orDefaultData liftedUserM = liftedUserM >>= maybe defaultUserData pure
