module GenTypesDemo.Component.Routing where

import Prelude

import Data.Either (either)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import GenTypesDemo.Data.Route (Route(..), routeCodec)
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.PushState as PushState
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)
import Web.Router as Router
import Web.Router.Driver.PushState as Driver

type RoutingIO
  = { read :: Effect Route
    , emitter :: HS.Emitter Route
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    , reload :: Effect Unit
    }

mkRoutingManager :: Effect (RoutingIO /\ React.JSX)
mkRoutingManager = do
  interface <- PushState.makeInterface
  { path } <- interface.locationState
  value <- Ref.new $ either (const Error) identity $ parse routeCodec path
  { emitter, listener } <- HS.create
  let
    driver = Driver.makeDriver_ (parse routeCodec) (print routeCodec) interface
  router <-
    Router.makeRouter
      (\_ _ -> Router.continue)
      ( case _ of
          Router.Resolved _ route -> do
            newRoute <- Ref.modify (const route) value
            HS.notify listener newRoute
          _ -> pure unit
      )
      driver
  component <-
    React.component "Router" \_ -> React.do
      React.useEffectOnce do
        router.initialize
      pure React.empty
  pure
    ( { read: Ref.read value
      , emitter
      , navigate: router.navigate
      , redirect: router.redirect
      , reload: window >>= location >>= reload
      }
        /\ component unit
    )