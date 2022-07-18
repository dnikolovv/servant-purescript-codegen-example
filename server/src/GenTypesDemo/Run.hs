{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module GenTypesDemo.Run where

import Control.Monad.Except (ExceptT (ExceptT))
import qualified Data.Aeson as JSON
import Data.Generics.Product (HasType (typed))
import Data.List (sortOn)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import GenTypesDemo.API.Definition (UsersAPI, UsersTable, server)
import GenTypesDemo.API.DomainError (DomainError (NotFound, Unauthorized, ValidationError))
import GenTypesDemo.API.ManageUsers (ManageUsers (DeleteUser, GetAllUsers, GetUser, NewUser, UpdateUser), getUser)
import GenTypesDemo.API.Types (CreatedAt (CreatedAt), Email (Email), User (User), UserData (UserData), UserId (UserId), Username (Username), created, info)
import qualified GenTypesDemo.API.Types as API
import GenTypesDemo.API.Types.NotEmptyText (unsafeMkNotEmptyText)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Errors (errorMwDefJson)
import RIO
import qualified RIO.HashMap as HM
import RIO.Time (getCurrentTime)
import qualified Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, fromSecret)
import Servant.Server (Context (EmptyContext, (:.)), HasServer (hoistServerWithContext), ServerError (errBody, errHTTPCode, errHeaders), err400, err401, err404, serveWithContext)
import System.IO (print)

run :: IO ()
run = do
  initializeUsers
    >>= runSettings
      ( setPort port $
          setBeforeMainLoop
            (print $ "Running on port " <> show port)
            defaultSettings
      )
      . corsMiddleware allowedCors
      . errorMwDefJson
      . logStdoutDev
      . waiApp
  where
    waiApp users = do
      let jwtCfg = defaultJWTSettings (fromSecret . fromString $ "this secret is kept very very securely")
          cookieCfg = defaultCookieSettings
          context = cookieCfg :. jwtCfg :. EmptyContext

      serveWithContext usersApi context (hoistServerWithContext usersApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) (effToHandler users) server)
    port = 3005
    allowedCors = (["http://localhost:1234"], True)
    usersApi = Proxy @UsersAPI
    initializeUsers = do
      now <- CreatedAt <$> getCurrentTime
      newIORef $
        HM.fromList
          [ ( UserId $ unsafeUUIDFromText "0290ee1e-1a64-4ef6-89c1-f8cd3d6298a1",
              UserData
                (Email "1@email.com")
                (Username . unsafeMkNotEmptyText $ "one")
                now
            ),
            ( UserId $ unsafeUUIDFromText "993ba001-6d6d-49b2-bcfa-e00586382ce6",
              UserData
                (Email "2@email.com")
                (Username . unsafeMkNotEmptyText $ "two")
                now
            ),
            ( UserId $ unsafeUUIDFromText "6ab9869c-db81-46b6-ac7b-306d1f0be023",
              UserData
                (Email "3@email.com")
                (Username . unsafeMkNotEmptyText $ "three")
                now
            ),
            ( UserId $ unsafeUUIDFromText "dda3db68-744e-4250-803f-25168f9f8d87",
              UserData
                (Email "11@email.com")
                (Username . unsafeMkNotEmptyText $ "eleven")
                now
            )
          ]

    unsafeUUIDFromText = fromMaybe (error "nope") . UUID.fromText

effToHandler :: UsersTable -> Eff [ManageUsers, Error DomainError, IOE] a -> Servant.Handler a
effToHandler usersRef m = do
  result <- liftIO . runEff . runErrorNoCallStack @DomainError . runInMemoryUserStorage usersRef $ m

  Servant.Handler $ ExceptT (pure . mapLeft toServerError $ result)
  where
    toServerError = \case
      ValidationError t -> servantErrorWithText err400 t
      NotFound t -> servantErrorWithText err404 t
      Unauthorized -> servantErrorWithText err401 "Unauthorized."

runInMemoryUserStorage ::
  ( IOE :> es,
    Error DomainError :> es
  ) =>
  UsersTable ->
  Eff (ManageUsers : es) a ->
  Eff es a
runInMemoryUserStorage usersRef = interpret $ \_ -> \case
  GetAllUsers -> do
    sortOn (created . info) . map (uncurry User) . HM.toList <$> readIORef usersRef
  GetUser uId -> do
    userMay <- lookupUser uId
    case userMay of
      Just u -> pure $ User uId u
      Nothing ->
        throwError $ NotFound "User not found."
  NewUser email username -> do
    newUserId <- UserId <$> liftIO UUID.nextRandom
    now <- CreatedAt <$> getCurrentTime
    let userData = UserData email username now
    modifyIORef' usersRef $ HM.insert newUserId userData
    runInMemoryUserStorage usersRef (getUser newUserId)
  DeleteUser uId -> do
    modifyIORef' usersRef (HM.delete uId) >> pure ()
  UpdateUser uId newEmail newUsername -> do
    validate (isJust <$> lookupUser uId) $
      ValidationError "Unexisting user."

    modifyIORef' usersRef $
      HM.adjust
        ( \userData ->
            userData
              & typed @Email %~ (`fromMaybe` newEmail)
              & typed @Username %~ (`fromMaybe` newUsername)
        )
        uId
  where
    validate condition err =
      condition >>= \result ->
        if result
          then pure ()
          else throwError err

    lookupUser :: MonadIO m => UserId -> m (Maybe UserData)
    lookupUser uId = HM.lookup uId <$> readIORef usersRef

servantErrorWithText ::
  ServerError ->
  Text ->
  ServerError
servantErrorWithText sErr msg =
  sErr
    { errBody = errorBody (errHTTPCode sErr),
      errHeaders = [jsonHeaders]
    }
  where
    errorBody code = JSON.encode $ API.Error msg code

    jsonHeaders =
      (fromString "Content-Type", "application/json;charset=utf-8")

type SendCredentials = Bool

type Origins = ([Origin], SendCredentials)

corsMiddleware :: Origins -> Middleware
corsMiddleware origins = do
  cors $
    const $
      Just $
        simpleCorsResourcePolicy
          { corsMethods = ["PUT", "GET", "DELETE", "HEAD", "OPTIONS", "POST"],
            corsRequestHeaders = ["content-type", "authorization", "sentry-trace"],
            corsOrigins = Just origins
          }