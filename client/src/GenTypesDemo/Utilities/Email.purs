module GenTypesDemo.Utilities.Email (Email, mkEmail) where

import Prelude

import Data.Argonaut (JsonDecodeError(..), caseJsonString, encodeJson, fromString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Text.Email.Parser (EmailAddress)
import Text.Email.Parser as Email
import Text.Email.Validate (emailAddress)

newtype Email = Email EmailAddress

mkEmail :: String -> Maybe Email
mkEmail = map Email <<< emailAddress

instance Show Email where
  show (Email email) = Email.toString email

derive instance Eq Email

instance EncodeJson Email where
  encodeJson (Email email) = encodeJson <<< Email.toString $ email

instance DecodeJson Email where
  decodeJson = caseJsonString (Left $ TypeMismatch "string") $ \strValue ->
    case emailAddress strValue of
      Just validEmail -> Right (Email validEmail)
      Nothing -> Left $ UnexpectedValue $ fromString $ strValue <> " is not a valid email."