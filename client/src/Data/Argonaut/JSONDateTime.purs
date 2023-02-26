module Data.Argonaut.JSONDateTime where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.DateTime (DateTime)
import Data.DateTime as Date
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time as Time
import Effect (Effect)
import Effect.Exception (try)
import Effect.Now (nowDateTime)
import Effect.Unsafe (unsafePerformEffect)
import StringParser (Parser, runParser)
import StringParser as Parser
import StringParser.CodePoints (regex)

newtype JSONDateTime
  = JSONDateTime DateTime

derive instance genericJSONDateTime :: Generic JSONDateTime _

derive newtype instance eqJSONDateTime :: Eq JSONDateTime

derive newtype instance ordJSONDateTime :: Ord JSONDateTime

formatMonthYear :: JSONDateTime -> String
formatMonthYear (JSONDateTime dt) =
  let
    date = Date.date dt

    month = show $ Date.month date

    year = show <<< fromEnum $ Date.year date
  in
    month <> " " <> year

getJSONDateTime :: JSONDateTime -> DateTime
getJSONDateTime (JSONDateTime x) = x

nowJSONDateTime :: Effect JSONDateTime
nowJSONDateTime = JSONDateTime <$> nowDateTime

instance showJSONDateTime :: Show JSONDateTime where
  show (JSONDateTime x) =
    let
      date'' = DateTime.date x

      time'' = DateTime.time x

      date' =
        JSDate.jsdate
          { year: Int.toNumber $ fromEnum $ Date.year date''
          , month: Int.toNumber $ fromEnum (Date.month date'') - 1
          , day: Int.toNumber $ fromEnum $ Date.day date''
          , hour: Int.toNumber $ fromEnum $ Time.hour time''
          , minute: Int.toNumber $ fromEnum $ Time.minute time''
          , second: Int.toNumber $ fromEnum $ Time.second time''
          , millisecond: Int.toNumber $ fromEnum $ Time.millisecond time''
          }

      s = unsafePerformEffect $ JSDate.toISOString date'

      y = case String.stripSuffix (String.Pattern "Z") s of
        Nothing -> s
        Just s' -> case String.stripSuffix (String.Pattern "0") s' of
          Nothing -> s' <> "Z"
          Just s'' -> case String.stripSuffix (String.Pattern "0") s'' of
            Nothing -> s'' <> "Z"
            Just s''' -> case String.stripSuffix (String.Pattern ".0") s''' of
              Nothing -> s''' <> "Z"
              Just s'''' -> s'''' <> "Z"
    in
      y

instance encodeJsonJSONDateTime :: EncodeJson JSONDateTime where
  encodeJson = encodeJson <<< show

jsonDateTimeParser :: Parser JSONDateTime
jsonDateTimeParser = do
  s <- regex "\\d{4}-[01]\\d-[0-3]\\dT[0-2]\\d:[0-5]\\d:[0-5]\\d\\.\\d+([+-][0-2]\\d:[0-5]\\d|Z)"
  case unsafePerformEffect $ try $ JSDate.parse s of
    Left _ -> Parser.fail "Not a datetime"
    Right x -> case JSDate.toDateTime x of
      Nothing -> Parser.fail "Not a datetime"
      Just y -> pure (JSONDateTime y)

instance decodeJsonJSONDateTime :: DecodeJson JSONDateTime where
  decodeJson json = do
    s <- decodeJson json
    case runParser jsonDateTimeParser s of
      Left _ -> Left $ TypeMismatch "Not a datetime"
      Right x -> pure x
