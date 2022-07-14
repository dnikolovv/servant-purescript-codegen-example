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
import GenTypesDemo.API.Types
import RIO
import qualified RIO.HashMap as HM
import RIO.State
import Servant

type UsersAPI =
  "get" :> Capture "userId" UserId :> Get '[JSON] User
    :<|> "post" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
    :<|> "put" :> Capture "userId" UserId :> ReqBody '[JSON] UpdateUserRequest :> PutNoContent
    :<|> "delete" :> Capture "userId" UserId :> DeleteNoContent

type UsersTable = HashMap UserId UserData

newtype AppM a = AppM {runAppM :: StateT UsersTable Servant.Handler a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState UsersTable,
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
      userMay <- gets (HM.lookup uId)
      case userMay of
        Just u -> pure $ User uId u
        Nothing -> throwError err404
    createUser CreateUserRequest {..} = do
      newUserId <- UserId <$> undefined
      let userData = UserData email username
      modify' $ HM.insert newUserId userData
      pure $ User newUserId userData
    updateUser uId UpdateUserRequest {..} = do
      validate (isJust <$> gets (HM.lookup uId)) err400

      modify' $
        HM.adjust
          ( \userData ->
              userData
                & typed @Email %~ (`fromMaybe` newEmail)
                & typed @Username %~ (`fromMaybe` newUsername)
          )
          uId

      pure NoContent
    deleteUser uId =
      modify' (HM.delete uId) >> pure NoContent

    validate :: AppM Bool -> ServerError -> AppM ()
    validate condition err =
      condition >>= \result ->
        if result
          then pure ()
          else throwError err