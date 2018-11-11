{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Car where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Time.Time

data Car = Car
  { __position :: Position
  , __linearMotion :: LinearMotion
  } deriving (Eq, Show)

makeClassy ''Car

instance HasPosition Car where
  position = _position

instance HasLinearMotion Car where
  linearMotion = _linearMotion

initialCar :: Car
initialCar = Car
    { __position = (V2 64 64)
    , __linearMotion = LinearMotion.initialLinearMotion
      { __direction = East
      , _speed = carSpeed
      }
    }
  where
    -- | How far the car moves in one second
    carSpeed :: Speed
    carSpeed = 64 * (1 / secondsPerTile)
      where secondsPerTile = 0.5

step :: (Time m) => Car -> m Car
step = LinearMotion.stepEff
