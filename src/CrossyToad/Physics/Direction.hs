{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Direction where

import Control.Lens
import Linear.V2

-- | The direction an object is facing
data Direction = North | East | South | West
  deriving (Eq, Show, Ord)

makeClassy ''Direction
makeClassyPrisms ''Direction

-- | Returns a unit vector corresponding to the direction
unitVector :: Direction -> V2 Float
unitVector North = V2 0 (-1)
unitVector East = V2 1 0
unitVector South = V2 0 1
unitVector West = V2 (-1) 0
