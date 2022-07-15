module GenTypesDemo.Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', default, root)
import Routing.Duplex.Generic (noArgs, sum)

data Route
  = Home
  | Error

derive instance Generic Route _

derive instance Eq Route

routeCodec :: RouteDuplex' Route
routeCodec =
  default Error
    $ root
    $ sum
        { "Home": noArgs
        , "Error": noArgs
        }