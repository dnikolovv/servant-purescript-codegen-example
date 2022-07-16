{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GenTypesDemo.API.Definition where

import Control.Monad.Error.Class
import qualified Data.Aeson as JSON
import Data.Generics.Product (HasType (typed))
import Data.List (sortOn)
import Data.UUID.V4 (nextRandom)
import GenTypesDemo.API.Auth (APIUser)
import GenTypesDemo.API.Types
import RIO
import qualified RIO.HashMap as HM
import RIO.Time (getCurrentTime)
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
                    :<|> Capture "userId" UserId :> ReqBody '[JSON] UpdateUserRequest :> PutNoContent
                )
         )

type ProtectedAPI =
  Auth '[JWT] APIUser
    :> ("user" :> Capture "userId" UserId :> Delete '[JSON] ())

type UsersTable = IORef (HashMap UserId UserData)

newtype AppM a = AppM {runAppM :: ReaderT UsersTable Servant.Handler a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader UsersTable,
      MonadError ServerError
    )

server :: ServerT UsersAPI AppM
server =
  publicServer
    :<|> protectedServer
  where
    publicServer =
      getAllUsers
        :<|> getUser
        :<|> createUser
        :<|> updateUser

    protectedServer (Authenticated _) uId =
      deleteUser uId
    protectedServer _ _ =
      throwError $ servantErrorWithText err401 "Unauthorized."

    getAllUsers =
      sortOn (created . info) . map (uncurry User) . HM.toList <$> (ask >>= readIORef)
    getUser uId = do
      userMay <- lookupUser uId
      case userMay of
        Just u -> pure $ User uId u
        Nothing ->
          throwError $
            servantErrorWithText err404 "User not found."
    createUser CreateUserRequest {..} = do
      newUserId <- UserId <$> liftIO nextRandom
      now <- CreatedAt <$> getCurrentTime
      let userData = UserData email username now
      usersRef <- ask
      modifyIORef' usersRef $ HM.insert newUserId userData
      getUser newUserId
    updateUser uId UpdateUserRequest {..} = do
      validate (isJust <$> lookupUser uId) $
        servantErrorWithText err400 "Unexisting user."

      usersRef <- ask
      modifyIORef' usersRef $
        HM.adjust
          ( \userData ->
              userData
                & typed @Email %~ (`fromMaybe` newEmail)
                & typed @Username %~ (`fromMaybe` newUsername)
          )
          uId

      pure NoContent
    deleteUser uId =
      ask >>= \usersRef -> modifyIORef' usersRef (HM.delete uId) >> pure ()

    lookupUser :: UserId -> AppM (Maybe UserData)
    lookupUser uId = ask >>= readIORef >>= pure . HM.lookup uId

    validate :: AppM Bool -> ServerError -> AppM ()
    validate condition err =
      condition >>= \result ->
        if result
          then pure ()
          else throwError err

    servantErrorWithText ::
      Servant.ServerError ->
      Text ->
      Servant.ServerError
    servantErrorWithText sErr msg =
      sErr
        { errBody = errorBody (errHTTPCode sErr),
          errHeaders = [jsonHeaders]
        }
      where
        errorBody code = JSON.encode $ Error msg code

    jsonHeaders =
      (fromString "Content-Type", "application/json;charset=utf-8")