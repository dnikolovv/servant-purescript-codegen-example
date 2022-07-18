{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GenTypesDemo.API.ManageUsers where

import Effectful
import Effectful.TH
import GenTypesDemo.API.Types (Email, User, UserId, Username)
import RIO

data ManageUsers :: Effect where
  GetAllUsers :: ManageUsers m [User]
  GetUser :: UserId -> ManageUsers m User
  NewUser :: Email -> Username -> ManageUsers m User
  DeleteUser :: UserId -> ManageUsers m ()
  UpdateUser :: UserId -> Maybe Email -> Maybe Username -> ManageUsers m ()

type instance DispatchOf ManageUsers = 'Dynamic

makeEffect ''ManageUsers