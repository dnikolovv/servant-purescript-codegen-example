module GenTypesDemo.Capability.Log where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import React.Halo (HaloM)
import GenTypesDemo.Capability.Now (class Now)
import GenTypesDemo.Data.Log (Log, LogReason(..), mkLog)

class
  Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: LogMessages m => LogMessages (HaloM props state action m) where
  logMessage = lift <<< logMessage

log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

-- | Log a message for debugging purposes
logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

-- | Log a message to convey non-error information
logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

-- | Log a message as a warning
logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

-- | Log a message as an error
logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error

-- | Hush a monadic action by logging the error, leaving it open why the error is being logged
logHush :: forall m a. LogMessages m => Now m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action
    >>= case _ of
        Left e -> case reason of
          Debug -> logDebug e *> pure Nothing
          Info -> logInfo e *> pure Nothing
          Warn -> logWarn e *> pure Nothing
          Error -> logError e *> pure Nothing
        Right v -> pure $ Just v

-- | Hush a monadic action by logging the error in debug mode
debugHush :: forall m a. LogMessages m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
