module Effects.UsersDB where

import qualified Data.Hashable as H

newtype UserData =
  UserData
    { getEchoMultiplier :: Int
    }

class Monad m =>
      MonadUsersDB m
  where
  defaultUserData :: m UserData
  getUserData :: H.Hashable u => u -> m UserData
  modifyUserData :: H.Hashable u => u -> (UserData -> UserData) -> m ()
