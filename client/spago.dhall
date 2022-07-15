{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "types-generation"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "bifunctors"
  , "bytestrings"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "email-validate"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "foreign-object"
  , "formatters"
  , "functions"
  , "halogen-subscriptions"
  , "http-methods"
  , "integers"
  , "js-date"
  , "js-timers"
  , "json-helpers"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "partial"
  , "pipes"
  , "prelude"
  , "profunctor-lenses"
  , "promises"
  , "quotient"
  , "random"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "react-halo"
  , "refs"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "servant-support"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  , "unordered-collections"
  , "unsafe-coerce"
  , "uri"
  , "uuid"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-router"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}