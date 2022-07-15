module GenTypesDemo.API.Error where

import Prelude
import Data.Argonaut (Json, decodeJson, printJsonDecodeError, stringify)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import GenTypesDemo.API.Types (Error)
import Servant.PureScript (AjaxError(..), printAjaxError)

data APIError
  = APIError (AjaxError JsonDecodeError Json)
  | UnauthorizedAccess

data APIErrorContents
  = ValidationError Error
  | UnknownError (Maybe Json)

withErrorContents :: forall m. Monad m => APIError -> (APIErrorContents -> m Unit) -> m Unit
withErrorContents (APIError (AjaxError { response })) action = case _.body <$> response of
  Just res -> case decodeJson res of
    Right apiError -> action (ValidationError apiError)
    Left _otherError -> action (UnknownError $ Just res)
  Nothing -> action (UnknownError Nothing)

withErrorContents _ _ = pure unit

printAPIError :: APIError -> String
printAPIError (APIError ajax) = printAjaxError stringify printJsonDecodeError ajax

printAPIError UnauthorizedAccess = "Unauthorized access."
