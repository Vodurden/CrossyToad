module CrossyToad.Physics.Speed where

import Control.Lens

import CrossyToad.Time.Seconds

-- | How fast an object moves per second in pixels
type Speed = Double

class HasSpeed a where
  speed :: Lens' a Speed

instance HasSpeed Speed where speed = id

secondsPerTile :: Seconds -> Speed
secondsPerTile seconds = 64 * (1 / seconds)