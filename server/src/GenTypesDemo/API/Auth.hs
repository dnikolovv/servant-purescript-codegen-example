{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GenTypesDemo.API.Auth where

import Crypto.JWT (emptyClaimsSet, unregisteredClaims)
import Data.Aeson
import qualified Data.Aeson as JSON
import GenTypesDemo.API.Types (Username)
import RIO
import Servant (FromHttpApiData, ToHttpApiData)
import Servant.Auth.Server (FromJWT (decodeJWT), ToJWT (encodeJWT))

newtype AuthorizationHeader = AuthorizationHeader Text
  deriving (Generic)
  deriving newtype (ToHttpApiData, FromHttpApiData)
  deriving anyclass (FromJSON, ToJSON)

newtype APIUser = APIUser
  { username :: Username
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance ToJWT APIUser where
  encodeJWT a = do
    let (Object userAsHashmap) = toJSON a
    set unregisteredClaims userAsHashmap emptyClaimsSet

instance FromJWT APIUser where
  decodeJWT m =
    case fromJSON (m ^. unregisteredClaims . to JSON.Object) of
      Success user -> pure user
      Error _ -> Left "Unable to deserialize an APIUser from the given JWT."