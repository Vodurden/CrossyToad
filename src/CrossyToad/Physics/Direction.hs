{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Direction where

import Control.Lens
import Data.Degrees
import Linear.V2

-- | The direction an object is facing
data Direction = North | East | South | West
  deriving (Eq, Show, Ord)

makeClassy ''Direction
makeClassyPrisms ''Direction

-- | Returns a unit vector corresponding to the direction
unitVector :: Direction -> V2 Double
unitVector North = V2 0 (-1)
unitVector East = V2 1 0
unitVector South = V2 0 1
unitVector West = V2 (-1) 0

degrees :: Direction -> Degrees
degrees North = 0
degrees East = 90
degrees South = 180
degrees West = 270
