{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Car
  ( Car(..)
  , HasCar(..)
  , initialCar
  , step
  , render
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Time.Time
import           CrossyToad.Renderer.Renderer

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

render :: (Renderer m) => Car -> m ()
render car' = drawCar (car' ^. position)
