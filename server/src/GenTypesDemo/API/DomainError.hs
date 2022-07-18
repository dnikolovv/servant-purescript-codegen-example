module GenTypesDemo.API.DomainError where

import RIO (Text)

data DomainError
  = ValidationError Text
  | NotFound Text
  | Unauthorized