{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GenTypesDemo.API.CodeGen where

import GenTypesDemo.API.Definition (UsersAPI)
import GenTypesDemo.API.Types (CreateUserRequest, CreatedAt, Email, Error, UpdateUserRequest, User, UserData, UserId, Username)
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes
import RIO
import Servant.PureScript (HasBridge (..), Settings, addTypes, defaultSettings, generateWithSettings)

codegen :: String -> IO ()
codegen path =
  genPureScriptTypes path
    >> genServant path

genServant :: String -> IO ()
genServant dir =
  generateWithSettings
    mySettings
    dir
    myBridgeProxy
    (Proxy @UsersAPI)

data MyBridge

instance HasBridge MyBridge where
  languageBridge _ = buildBridge bridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

mySettings :: Settings
mySettings =
  defaultSettings
    & addTypes myTypes

genPureScriptTypes :: String -> IO ()
genPureScriptTypes path =
  writePSTypes
    path
    (buildBridge bridge)
    myTypes

bridge :: BridgePart
bridge =
  uuidBridge
    <|> emailBridge
    <|> notEmptyTextBridge
    <|> utcTimeBridge
    <|> defaultBridge

-- We'll translate our NotEmptyText to plain text
notEmptyTextBridge :: BridgePart
notEmptyTextBridge = do
  typeName ^== "NotEmptyText"
  pure psString

-- But we can also fall back to a strongly typed PureScript version
-- and force client-side validation to be performed
emailBridge :: BridgePart
emailBridge = do
  typeName ^== "Email"
  -- Our own custom Email type on the front-end
  pure $ TypeInfo "" "GenTypesDemo.Utilities.Email" "Email" []

uuidBridge :: BridgePart
uuidBridge = do
  typeName ^== "UUID"
  typeModule ^== "Data.UUID" <|> typeModule ^== "Data.UUID.Types.Internal"
  pure psUUID

psUUID :: PSType
psUUID = TypeInfo "web-common" "Data.UUID.Argonaut" "UUID" []

utcTimeBridge :: BridgePart
utcTimeBridge = do
  typeName ^== "UTCTime"
  pure psUTCTime

psUTCTime :: PSType
psUTCTime = TypeInfo "haskell-iso" "Data.Argonaut.JSONDateTime" "JSONDateTime" []

myTypes :: [SumType 'Haskell]
myTypes =
  [ genericShow $ equal $ argonaut $ mkSumType @CreateUserRequest,
    order $ genericShow $ equal $ argonaut $ mkSumType @Username,
    order $ genericShow $ equal $ argonaut $ mkSumType @UserId,
    genericShow $ equal $ argonaut $ mkSumType @UserData,
    order $ genericShow $ equal $ argonaut $ mkSumType @CreatedAt,
    genericShow $ equal $ argonaut $ mkSumType @User,
    genericShow $ equal $ argonaut $ mkSumType @UpdateUserRequest,
    order $ genericShow $ equal $ argonaut $ mkSumType @Error
  ]