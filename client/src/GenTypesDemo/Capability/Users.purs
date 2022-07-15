module GenTypesDemo.Capability.Users where

import Prelude

import Control.Monad.Cont (lift)
import Data.Either (Either)
import GenTypesDemo.API.Error (APIError(..))
import GenTypesDemo.API.Types (CreateUserRequest(..), User(..), UserId(..))
import React.Halo (HaloM)

class
  Monad m <= MonadUsers m where
  listUsers :: m (Either APIError (Array User))
  newUser :: CreateUserRequest -> m (Either APIError User)
  deleteUser :: UserId -> m (Either APIError Unit)

instance monadUsersHalogenM :: MonadUsers m => MonadUsers (HaloM st act slots msg m) where
  listUsers = lift listUsers
  newUser = lift <<< newUser
  deleteUser = lift <<< deleteUser
