{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenTypesDemo.API.Types where

import Data.Aeson
import RIO
import Servant (FromHttpApiData)

data CreateUserRequest = CreateUserRequest
  { email :: Email,
    username :: Username
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data User = User
  { id :: UserId,
    info :: UserData
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data UserData = UserData
  { email :: Email,
    username :: Username
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data UpdateUserRequest = UpdateUserRequest
  { newEmail :: Maybe Email,
    newUsername :: Maybe Username
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UserId = UserId Int
  deriving (Generic)
  deriving newtype (Eq, Hashable, ToJSON, FromJSON, FromHttpApiData)

newtype Email = Email Text
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON)

newtype Username = Username Text
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON)