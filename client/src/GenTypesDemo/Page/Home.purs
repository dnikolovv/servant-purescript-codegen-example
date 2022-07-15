module GenTypesDemo.Page.Home where

import Prelude
import Prelude
import Control.Monad.State (get, modify_)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Function.Uncurried (mkFn2)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut as UUID
import Debug (traceM)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
import GenTypesDemo.API.Error (APIError(..), printAPIError)
import GenTypesDemo.API.Types (CreateUserRequest(..), User(..), UserData(..), UserId(..), Username(..))
import GenTypesDemo.Capability.Halo (class MonadHalo, component)
import GenTypesDemo.Capability.Log (class LogMessages, logError)
import GenTypesDemo.Capability.Now (class Now)
import GenTypesDemo.Capability.Users (class MonadUsers, deleteUser, listUsers, newUser)
import GenTypesDemo.Component.NewUserRow (mkNewUserRow)
import GenTypesDemo.Config as Config
import Network.RemoteData (RemoteData)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React
import React.Halo as Halo
import Web.HTML.HTMLInputElement (placeholder)

type Props
  = {}

type State
  = { users :: RemoteData APIError (Array User)
    }

data Action
  = Initialize
  | AddUser CreateUserRequest
  | DeleteUser UserId

mkHomePage ::
  forall m.
  MonadEffect m =>
  MonadUsers m =>
  Now m =>
  LogMessages m =>
  MonadHalo m =>
  m (Props -> React.JSX)
mkHomePage = do
  newUserRow <- mkNewUserRow
  component "HomePage" { context, initialState, eval, render: render newUserRow }
  where
  context _ = pure unit

  initialState _ _ =
    { usersR: RemoteData.NotAsked
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      modify_ _ { usersR = RemoteData.Loading }
      usersR <- listUsers
      modify_ _ { usersR = RemoteData.fromEither usersR }
    AddUser newUserRequest -> do
      result <- newUser newUserRequest
      case result of
        Right u ->
          modify_
            $ \currentState ->
                let
                  updatedUsers = (\users -> users <> [ u ]) <$> currentState.usersR
                in
                  currentState { usersR = updatedUsers }
        Left err -> do
          traceM err
          logError "Could not add a new user."
    DeleteUser uId -> do
      result <- deleteUser uId
      case result of
        Right _ ->
          modify_
            $ \currentState ->
                let
                  updatedUsers = Array.filter (\(User { id }) -> id /= uId) <$> currentState.usersR
                in
                  currentState { usersR = updatedUsers }
        Left _err -> do
          logError "Could not delete user."
      pure unit

  render newUserRow { send, state: { usersR } } =
    R.section
      { className: "w-full h-screen bg-blue-50 flex flex-col items-center justify-center"
      , children:
          [ case usersR of
              RemoteData.Success users ->
                React.fragment
                  [ R.table
                      { className: "table-auto border bg-white border-blue-100 rounded-lg border-separate"
                      , children:
                          [ R.thead
                              { children:
                                  [ R.tr
                                      { children:
                                          [ headerEl "User Id"
                                          , headerEl "Username"
                                          , headerEl "Email"
                                          , headerEl "Created At"
                                          , headerEl "Actions"
                                          ]
                                      }
                                  ]
                              }
                          , R.tbody
                              { children: map toUserRow users
                              }
                          ]
                      }
                  , newUserRow { onSubmit: send <<< AddUser }
                  ]
              RemoteData.Loading -> message "Loading stuff..."
              RemoteData.Failure err -> message $ "Error! Make sure the API is running on " <> Config.apiUrl <> "."
              RemoteData.NotAsked -> message "Preparing the world..."
          ]
      }
    where
    message m =
      R.section
        { className: "rounded-lg bg-white p-4"
        , children:
            [ R.text m
            ]
        }

    toUserRow (User { id, info: (UserData { created, email, username }) }) =
      R.tr
        { className: ""
        , children:
            [ cell (UUID.toString <<< unwrap $ id)
            , cell (unwrap username)
            , cell (show email)
            , cell (show <<< unwrap $ created)
            , actions id
            ]
        }

    actions id =
      R.td
        { className: ""
        , children: [ delete ]
        }
      where
      delete =
        R.button
          { className: "p-2 text-white bg-red-4 rounded-md"
          , onClick: handler_ (send $ DeleteUser id)
          , children: [ R.text "Delete" ]
          }

    cell t =
      R.td
        { className: "p-6 text-gray-9"
        , children:
            [ R.text t
            ]
        }

    headerEl t =
      R.th
        { className: "p-6 text-gray-9 font-bold"
        , children: [ R.text t ]
        }
