module Data.UUID.Argonaut where

import Prelude

import Data.Argonaut.Aeson (maybeToEither)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.UUID as U
import Effect (Effect)
import Servant.PureScript (class ToPathSegment)

newtype UUID
  = UUID U.UUID

derive instance newtypeUUID :: Newtype UUID _

derive instance genericUUID :: Generic UUID _

derive instance eqUUID :: Eq UUID

derive instance ordUUID :: Ord UUID

instance showUUID :: Show UUID where
  show (UUID uuid) = U.toString uuid

instance ToPathSegment UUID where
  toPathSegment (UUID uuid) = U.toString uuid

instance encodeJsonUUID :: EncodeJson UUID where
  encodeJson = encodeString <<< U.toString <<< unwrap

instance decodeJsonUUID :: DecodeJson UUID where
  decodeJson =
    map UUID
      <<< maybeToEither (TypeMismatch "String in UUID format")
      <<< U.parseUUID
      <=< decodeString

_UUID :: Iso' UUID U.UUID
_UUID = _Newtype

emptyUUID :: UUID
emptyUUID = UUID U.emptyUUID

genUUID :: Effect UUID
genUUID = UUID <$> U.genUUID

parseUUID :: String -> Maybe UUID
parseUUID = map UUID <<< U.parseUUID

genv3UUID :: String -> UUID -> UUID
genv3UUID s = UUID <<< U.genv3UUID s <<< unwrap

genv5UUID :: String -> UUID -> UUID
genv5UUID s = UUID <<< U.genv5UUID s <<< unwrap

toString :: UUID -> String
toString = U.toString <<< unwrap
