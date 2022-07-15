module GenTypesDemo.Component.NewUserRow where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import GenTypesDemo.API.Types (CreateUserRequest(..), Username(..))
import GenTypesDemo.Utilities.Email (Email, mkEmail)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React

type Props
  = { onSubmit :: CreateUserRequest -> Effect Unit
    }

mkNewUserRow ::
  forall m.
  MonadEffect m =>
  m (Props -> React.JSX)
mkNewUserRow =
  liftEffect
    $ React.component "NewUserRow"
    $ \{ onSubmit } -> React.do
        (username /\ setUsername) <- React.useState ""
        (email /\ setEmail) <- React.useState ""
        (validationError /\ setValidationError) <- React.useState Nothing
        let
          clearFields = do
            setUsername (const "")
            setEmail (const "")
            setValidationError (const Nothing)

          handleSubmit = do
            case mkEmail email of
              Just validEmail -> do
                setValidationError (const Nothing)
                clearFields
                onSubmit (CreateUserRequest { username: Username username, email: validEmail })
              Nothing -> setValidationError (const $ Just "Invalid email.")
        pure
          $ R.div
              { className: "mt-8 space-y-4 flex flex-col bg-white rounded-lg border-blue-100 p-2"
              , children:
                  [ case validationError of
                      Just err ->
                        R.div
                          { className: "bg-red-4 text-white p-2 rounded-lg"
                          , children: [ R.text err ]
                          }
                      Nothing -> React.empty
                  , R.div
                      { className: "space-x-4 flex flex-row items-center"
                      , children:
                          [ R.input
                              { type: "text"
                              , className: "p-2 border border-slate-100 rounded-md"
                              , placeholder: "Username"
                              , value: username
                              , onChange: handler targetValue (\val -> setUsername (const <<< fromMaybe "" $ val))
                              }
                          , R.input
                              { type: "text"
                              , className: "p-2 border border-slate-100 rounded-md"
                              , placeholder: "Email"
                              , value: email
                              , onChange: handler targetValue (\val -> setEmail (const <<< fromMaybe "" $ val))
                              }
                          , R.button
                              { className: "p-2 bg-gold-4 rounded-md text-gray-9"
                              , onClick: handler_ handleSubmit
                              , children:
                                  [ R.text "Add User"
                                  ]
                              }
                          ]
                      }
                  ]
              }
