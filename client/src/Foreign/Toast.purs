module Foreign.Toast (toastContainer, toast, toastError) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (ReactComponent)
import React.Basic.Hooks as React

type ToastMessage
  = String

toastContainer :: React.JSX
toastContainer = React.element toastContainer_ {}

-- TODO: MonadToast?
toast :: forall m. MonadEffect m => ToastMessage -> m Unit
toast msg = liftEffect $ runEffectFn1 toast_ msg

toastError :: forall m. MonadEffect m => ToastMessage -> m Unit
toastError msg = liftEffect $ runEffectFn1 toastError_ msg

foreign import toastContainer_ :: ReactComponent {}

foreign import toast_ :: EffectFn1 ToastMessage Unit

foreign import toastError_ :: EffectFn1 ToastMessage Unit