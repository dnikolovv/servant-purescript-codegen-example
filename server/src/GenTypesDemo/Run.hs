{-# LANGUAGE TypeApplications #-}

module GenTypesDemo.Run where

import GenTypesDemo.API.Definition (AppM (runAppM), UsersAPI, server)
import GenTypesDemo.API.Types (Email (Email), User (User), UserData (UserData), UserId (UserId), Username (Username))
import RIO
import qualified RIO.HashMap as HM
import RIO.State (evalStateT)
import Servant.Server (hoistServer, serve)

run :: IO ()
run = do
  undefined
  where
    waiApp = serve usersApi (hoistServer usersApi (flip evalStateT initialUsers . runAppM) server)
    usersApi = Proxy @UsersAPI
    initialUsers =
      HM.fromList
        [ ( UserId 1,
            UserData
              (Email "1@email.com")
              (Username "one")
          ),
          ( UserId 2,
            UserData
              (Email "2@email.com")
              (Username "two")
          ),
          ( UserId 3,
            UserData
              (Email "3@email.com")
              (Username "three")
          ),
          ( UserId 11,
            UserData
              (Email "11@email.com")
              (Username "eleven")
          )
        ]