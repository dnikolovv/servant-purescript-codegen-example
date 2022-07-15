module GenTypesDemo.Capability.Global where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import GenTypesDemo.API.Error (APIError(..))
import Halogen.Subscription as HS
import React.Halo (HaloM)
import React.Halo as Halo

data GlobalAction = UseMeForIssuingMassCommands

data GlobalEvent =
  APIErrorOccurred APIError

type GlobalIO
  = { eventsEmitter :: HS.Emitter GlobalEvent
    , eventsListener :: HS.Listener GlobalEvent
    , actionsEmitter :: HS.Emitter GlobalAction
    , actionsListener :: HS.Listener GlobalAction
    }

class Monad m <= MonadGlobal m where
  emitGlobalAction :: GlobalAction -> m Unit
  emitGlobalEvent :: GlobalEvent -> m Unit
  getGlobalEventsEmitter :: m (HS.Emitter GlobalEvent)
  getGlobalActionsEmitter :: m (HS.Emitter GlobalAction)

instance MonadGlobal m => MonadGlobal (HaloM props ctx state action m) where
  emitGlobalAction = lift <<< emitGlobalAction
  emitGlobalEvent = lift <<< emitGlobalEvent
  getGlobalEventsEmitter = lift getGlobalEventsEmitter
  getGlobalActionsEmitter = lift getGlobalActionsEmitter

subscribeForGlobalEvents :: forall m props ctx state action. MonadGlobal m => (GlobalEvent -> action) -> HaloM props ctx state action m Unit
subscribeForGlobalEvents f = do
  emitter <- lift getGlobalEventsEmitter
  void $ Halo.subscribe $ f <$> emitter

subscribeForGlobalActions :: forall m props ctx state action. MonadGlobal m => (GlobalAction -> action) -> HaloM props ctx state action m Unit
subscribeForGlobalActions f = do
  emitter <- lift getGlobalActionsEmitter
  void $ Halo.subscribe $ f <$> emitter