module CrossyToad.Game.Entity
  ( Entity(..)
  , spawn
  ) where

import           Control.Lens

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Game.Car (HasCars(..))
import qualified CrossyToad.Game.Car as Car
import           CrossyToad.Game.RiverLog (HasRiverLogs(..))
import qualified CrossyToad.Game.RiverLog as RiverLog

data Entity
  = Car
  | RiverLog
  deriving (Eq, Show)

spawn :: (HasCars s, HasRiverLogs s) => Entity -> Position -> Direction -> s -> s
spawn Car pos dir state = state & cars %~ (Car.mk pos dir :)
spawn RiverLog pos dir state = state & riverLogs %~ (RiverLog.mk pos dir :)
