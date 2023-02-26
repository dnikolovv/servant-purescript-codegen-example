{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230224/packages.dhall
        sha256:b9e82e6715e87e2a701e925d5d1414bff8f7e923172bf58c2d9d77b0fa81b578

let overrides = {=}

let jsonHelpers =
      { json-helpers =
        { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
        , repo =
            "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
        , version = "0ff78186a949722f37218046a09abdf27d77ecfe"
        }
      }

let servantSupport =
      { servant-support =
        { dependencies =
          [ "aff"
          , "affjax"
          , "argonaut"
          , "arrays"
          , "bifunctors"
          , "either"
          , "http-methods"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "prelude"
          , "psci-support"
          , "strings"
          , "transformers"
          , "tuples"
          , "uri"
          ]
        , repo = "https://github.com/input-output-hk/purescript-servant-support"
        , version = "61f85eb0657196d4bfc80ae4736d6a6d9ebd4529"
        }
      }

let webRouter =
      { web-router =
        { dependencies =
          [ "aff"
          , "effect"
          , "freet"
          , "indexed-monad"
          , "prelude"
          , "profunctor-lenses"
          , "routing"
          ]
        , repo = "https://github.com/robertdp/purescript-web-router.git"
        , version = "v0.3.0"
        }
      }

let webAudio =
      { web-storage =
        { dependencies = [ "web-events", "nullable" ]
        , repo = "https://github.com/purescript-web/purescript-web-storage.git"
        , version = "6b74461e136755db70c271dc898d51776363d7e2"
        }
      , webaudio =
        { dependencies =
          [ "effect"
          , "arraybuffer-types"
          , "arrays"
          , "maybe"
          , "aff"
          , "foldable-traversable"
          , "math"
          , "tuples"
          , "strings"
          , "lists"
          , "arraybuffer"
          ]
        , repo = "https://github.com/adkelley/purescript-webaudio.git"
        , version = "v0.2.1"
        }
      }

in      upstream
    //  overrides
    //  jsonHelpers
    //  servantSupport
    //  webRouter
    //  webAudio
