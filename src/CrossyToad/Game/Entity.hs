module CrossyToad.Game.Entity
  where

import           Control.Lens

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Game.Car (HasCars(..))
import qualified CrossyToad.Game.Car as Car

data Entity
  = Car
  | RiverLog
  deriving (Eq, Show)

spawn :: (HasCars s) => Entity -> Position -> Direction -> s -> s
spawn Car pos dir state = state & cars %~ (Car.mk pos dir :)
spawn RiverLog _ _ state = state
