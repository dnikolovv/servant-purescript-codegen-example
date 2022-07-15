module GenTypesDemo.Root where

import Prelude

import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Uncurried (runEffectFn1)
import Foreign.Toast (toastContainer)
import GenTypesDemo.Capability.Global (class MonadGlobal)
import GenTypesDemo.Capability.Halo (class MonadHalo, component)
import GenTypesDemo.Capability.Log (class LogMessages, logDebug, logInfo)
import GenTypesDemo.Capability.Now (class Now)
import GenTypesDemo.Capability.Routing (class MonadRouting, navigate)
import GenTypesDemo.Capability.Routing as Routing
import GenTypesDemo.Capability.Users (class MonadUsers)
import GenTypesDemo.Component.GlobalContext (mkGlobalContext)
import GenTypesDemo.Config as Config
import GenTypesDemo.Data.Route (Route(..))
import GenTypesDemo.Page.Home (mkHomePage)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import React.Halo as Halo

data Action
  = Initialize
  | UpdateRoute Route
  | Navigate Route

type Props
  = {}

mkRoot ::
  forall m.
  MonadAff m =>
  MonadGlobal m =>
  MonadRouting m =>
  MonadHalo m =>
  MonadUsers m =>
  Now m =>
  LogMessages m =>
  m (Props -> React.JSX)
mkRoot = do
  render <- mkRender
  component "Root" { context, initialState, eval, render }
  where
  context _ = pure unit

  initialState _ _ =
    { route: Error
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      -- routing
      handleAction <<< UpdateRoute =<< Routing.read
      Routing.subscribe UpdateRoute
    UpdateRoute route -> do
      modify_ _ { route = route }
    Navigate route -> do
      Routing.navigate route

  mkRender = do
    homePage <- mkHomePage
    globalContext <- mkGlobalContext
    pure
      $ \{ state } ->
          let
            contents = case state.route of
              Home -> homePage {}
              Error -> homePage {}
          in
            React.fragment
              [ globalContext unit
              , toastContainer
              , contents
              ]
