{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenTypesDemo.API.Types where

import Data.Aeson
import Data.UUID (UUID)
import RIO
import RIO.Time (UTCTime)
import Servant (FromHttpApiData)
import GenTypesDemo.API.Types.NotEmptyText (NotEmptyText)

data Error = Error
  { error :: Text
  , status :: Int
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    username :: Username,
    created :: CreatedAt
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

data UpdateUserRequest = UpdateUserRequest
  { newEmail :: Maybe Email,
    newUsername :: Maybe Username
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UserId = UserId UUID
  deriving (Generic)
  deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON, FromHttpApiData)

newtype CreatedAt = CreatedAt UTCTime
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON, Eq, Ord)

newtype Email = Email Text
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON)

newtype Username = Username NotEmptyText
  deriving (Generic)
  deriving newtype (Show, ToJSON, FromJSON)