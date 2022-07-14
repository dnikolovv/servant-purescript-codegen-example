{-# LANGUAGE TypeApplications #-}

module GenTypesDemo.Run where

import qualified Data.UUID as UUID
import GenTypesDemo.API.Definition (AppM (runAppM), UsersAPI, server)
import GenTypesDemo.API.Types (Email (Email), User (User), UserData (UserData), UserId (UserId), Username (Username))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import RIO
import qualified RIO.HashMap as HM
import RIO.State (evalStateT)
import Servant.Server (hoistServer, serve)
import System.IO (print)

run :: IO ()
run =
  initializeUsers
    >>= runSettings
      ( setPort port $
          setBeforeMainLoop
            (print $ "Running on port " <> show port)
            defaultSettings
      )
      . waiApp
  where
    waiApp users = serve usersApi (hoistServer usersApi (flip runReaderT users . runAppM) server)
    port = 3005
    usersApi = Proxy @UsersAPI
    initializeUsers =
      newIORef $
        HM.fromList
          [ ( UserId $ unsafeUUIDFromText "0290ee1e-1a64-4ef6-89c1-f8cd3d6298a1",
              UserData
                (Email "1@email.com")
                (Username "one")
            ),
            ( UserId $ unsafeUUIDFromText "993ba001-6d6d-49b2-bcfa-e00586382ce6",
              UserData
                (Email "2@email.com")
                (Username "two")
            ),
            ( UserId $ unsafeUUIDFromText "6ab9869c-db81-46b6-ac7b-306d1f0be023",
              UserData
                (Email "3@email.com")
                (Username "three")
            ),
            ( UserId $ unsafeUUIDFromText "dda3db68-744e-4250-803f-25168f9f8d87",
              UserData
                (Email "11@email.com")
                (Username "eleven")
            )
          ]

    unsafeUUIDFromText = fromMaybe (error "nope") . UUID.fromText