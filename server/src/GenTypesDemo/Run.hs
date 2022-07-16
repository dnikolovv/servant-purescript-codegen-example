{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module GenTypesDemo.Run where

import qualified Data.UUID as UUID
import GenTypesDemo.API.Definition (AppM (runAppM), UsersAPI, server)
import GenTypesDemo.API.Types (CreatedAt (CreatedAt), Email (Email), UserData (UserData), UserId (UserId), Username (Username))
import GenTypesDemo.API.Types.NotEmptyText (unsafeMkNotEmptyText)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Errors (errorMwDefJson)
import RIO
import qualified RIO.HashMap as HM
import RIO.Time (getCurrentTime)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, fromSecret)
import Servant.Server (Context (EmptyContext, (:.)), HasServer (hoistServerWithContext), serveWithContext)
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

      serveWithContext usersApi context (hoistServerWithContext usersApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) (flip runReaderT users . runAppM) server)
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