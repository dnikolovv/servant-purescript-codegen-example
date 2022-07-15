module Main where

import Prelude
import Data.Argonaut (encodeJson, stringify)
import Data.Argonaut.Decode (decodeJson, parseJson)
import Data.Array (notElem, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn1, runEffectFn1, runEffectFn2)
import GenTypesDemo.AppM (LogLevel(..), runAppM)
import GenTypesDemo.Capability.Global (GlobalIO)
import GenTypesDemo.Component.Routing as Routing
import GenTypesDemo.Config as Config
import GenTypesDemo.Root as Root
import Halogen.Subscription as HS
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Hooks as R
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.History (pushState)
import Web.HTML.Location (setHref)
import Web.HTML.Window (document, history, localStorage, location)
import Web.Storage.Storage (getItem, setItem)

main :: Effect Unit
main = do
  rootMay <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case rootMay of
    Just root -> do
      routing /\ routingManager <- Routing.mkRoutingManager
      let
        logLevel = case Config.nodeEnv of
          "production" -> Prod
          "development" -> Dev
          _ -> Dev
      globalIO <- mkGlobalIO
      launchAff_
        $ do
            rootComponent <- runAppM { routing, logLevel, globalIO } Root.mkRoot
            mainComponent <-
              liftEffect
                $ React.component "Main"
                $ \_ -> React.do
                    pure
                      $ R.fragment
                          [ routingManager
                          , rootComponent {}
                          ]
            liftEffect
              $ R.render (mainComponent unit) root
    Nothing -> log "No root element."

mkGlobalIO :: Effect GlobalIO
mkGlobalIO = create
  where
  create = do
    { emitter: actionsEmitter, listener: actionsListener } <- HS.create
    { emitter: eventsEmitter, listener: eventsListener } <- HS.create
    pure
      { eventsEmitter
      , eventsListener
      , actionsEmitter
      , actionsListener
      }
