{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module GenTypesDemo.API.Types.NotEmptyText
  ( NotEmptyText,
    unNotEmptyText,
    mkNotEmptyText,
    mkNotEmptyTextMay,
    unsafeMkNotEmptyText,
    pattern NotEmptyText,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    Value (Object, String),
    (.:),
  )
import Data.Aeson.Types (typeMismatch)
import GHC.Base (coerce)
import RIO
import RIO.Text (pack, strip, uncons)
import Text.Read (Read (readsPrec))

newtype NotEmptyText = MkNotEmptyText Text
  deriving newtype (ToJSON, Eq, Show)

unNotEmptyText :: NotEmptyText -> Text
unNotEmptyText = coerce

instance FromJSON NotEmptyText where
  parseJSON (String str) =
    case mkNotEmptyText str of
      Right text -> pure text
      Left err -> fail (show err)
  parseJSON other =
    typeMismatch "Object or String" other

pattern NotEmptyText :: Text -> NotEmptyText
pattern NotEmptyText text <- MkNotEmptyText text

instance Read NotEmptyText where
  readsPrec _ = readTextWrapper MkNotEmptyText

unsafeMkNotEmptyText :: Text -> NotEmptyText
unsafeMkNotEmptyText = fromMaybe (error "Tried to construct non-empty text value.") . mkNotEmptyTextMay

mkNotEmptyText :: Text -> Either Text NotEmptyText
mkNotEmptyText (uncons . strip -> Nothing) =
  Left "Should not be empty."
mkNotEmptyText text = Right $ MkNotEmptyText text

mkNotEmptyTextMay :: Text -> Maybe NotEmptyText
mkNotEmptyTextMay = eitherToMaybe . mkNotEmptyText

readTextWrapper :: IsString b => (Text -> a) -> String -> [(a, b)]
readTextWrapper constructor input = [(constructor (pack input), "")]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right val) = Just val
eitherToMaybe _ = Nothing