{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GenTypesDemo.API.Definition where

import Effectful (Eff)
import qualified Effectful as E
import qualified Effectful.Error.Static as ES
import GenTypesDemo.API.Auth (APIUser)
import GenTypesDemo.API.ManageUsers (ManageUsers, deleteUser, getAllUsers, getUser, newUser, updateUser)
import GenTypesDemo.API.Types
import RIO
import Servant
import Servant.Auth.Server

-- Known issues
-- NoContent endpoints will fail deserialization because the PureScript front-end will expect an [] to deserialize into Unit
-- Required QueryParams won't get properly serialized
-- newtypes with named fields don't get serialized properly e.g. Username = Username { unUsername :: Text }

type UsersAPI =
  PublicAPI
    :<|> ProtectedAPI

type PublicAPI =
  "users" :> Get '[JSON] [User]
    :<|> ( "user"
             :> ( Capture "userId" UserId :> Get '[JSON] User
                    :<|> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
                    :<|> Capture "userId" UserId :> ReqBody '[JSON] UpdateUserRequest :> Put '[JSON] ()
                )
         )

type ProtectedAPI =
  Auth '[JWT] APIUser
    :> ("user" :> Capture "userId" UserId :> Delete '[JSON] ())

type UsersTable = IORef (HashMap UserId UserData)

server ::
  ManageUsers E.:> es =>
  ES.Error ServerError E.:> es =>
  ServerT UsersAPI (Eff es)
server =
  publicServer
    :<|> protectedServer
  where
    publicServer =
      getAllUsers
        :<|> getUser
        :<|> (\CreateUserRequest {..} -> newUser email username)
        :<|> (\uId UpdateUserRequest {..} -> updateUser uId newEmail newUsername)

    protectedServer (Authenticated _) uId = do
      deleteUser uId
    protectedServer _ _ =
      ES.throwError err401