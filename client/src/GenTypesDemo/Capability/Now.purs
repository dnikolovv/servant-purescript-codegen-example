module GenTypesDemo.Capability.Now where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, DateTime, Time)
import Data.DateTime.Instant (Instant)
import React.Halo (HaloM)

class
  Monad m <= Now m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime

instance nowHalogenM :: Now m => Now (HaloM props state action m) where
  now = lift now
  nowDate = lift nowDate
  nowTime = lift nowTime
  nowDateTime = lift nowDateTime
