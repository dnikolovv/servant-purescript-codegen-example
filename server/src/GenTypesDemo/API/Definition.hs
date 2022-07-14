{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GenTypesDemo.API.Definition where

import Control.Monad.Error.Class
import Data.Generics.Product (HasType (typed))
import Data.UUID.V4 (nextRandom)
import GenTypesDemo.API.Types
import RIO
import qualified RIO.HashMap as HM
import RIO.State
import Servant
import System.Random (randomRIO)

type UsersAPI =
  "user"
    :> ( Capture "userId" UserId :> Get '[JSON] User
           :<|> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
           :<|> Capture "userId" UserId :> ReqBody '[JSON] UpdateUserRequest :> PutNoContent
           :<|> Capture "userId" UserId :> DeleteNoContent
       )

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
  getUser
    :<|> createUser
    :<|> updateUser
    :<|> deleteUser
  where
    getUser uId = do
      userMay <- lookupUser uId
      case userMay of
        Just u -> pure $ User uId u
        Nothing -> throwError err404
    createUser CreateUserRequest {..} = do
      newUserId <- UserId <$> liftIO nextRandom
      let userData = UserData email username
      usersRef <- ask
      modifyIORef' usersRef $ HM.insert newUserId userData
      getUser newUserId
    updateUser uId UpdateUserRequest {..} = do
      validate (isJust <$> lookupUser uId) err400

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
      ask >>= \usersRef -> modifyIORef' usersRef (HM.delete uId) >> pure NoContent

    lookupUser :: UserId -> AppM (Maybe UserData)
    lookupUser uId = ask >>= readIORef >>= pure . HM.lookup uId

    validate :: AppM Bool -> ServerError -> AppM ()
    validate condition err =
      condition >>= \result ->
        if result
          then pure ()
          else throwError err