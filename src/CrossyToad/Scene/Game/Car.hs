{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Car
  ( Car(..)
  , HasCar(..)
  , HasCars(..)
  , mk
  , step
  , stepAll
  , render
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Physics.CollisionBox (CollisionBox(..), HasCollisionBox(..))
import qualified CrossyToad.Physics.CollisionBox as CollisionBox
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Time.Time

data Car = Car
  { __position :: Position
  , __linearMotion :: LinearMotion
  , __collisionBox :: CollisionBox
  } deriving (Eq, Show)

makeClassy ''Car

class HasCars a where
  cars :: Lens' a [Car]

instance HasCars [Car] where
  cars = id

instance HasPosition Car where
  position = _position

instance HasLinearMotion Car where
  linearMotion = _linearMotion

instance HasCollisionBox Car where
  collisionBox = _collisionBox

mk :: Position -> Direction -> Car
mk pos dir = Car
    { __position = pos
    , __linearMotion = LinearMotion.mk dir carSpeed
    , __collisionBox = CollisionBox.mk (V2 64 64)
    }
  where
    -- | How far the car moves in one second
    carSpeed :: Speed
    carSpeed = 64 * (1 / secondsPerTile)
      where secondsPerTile = 0.5

stepAll :: (Time m, HasCars ent) => ent -> m ent
stepAll = (cars.traverse) step

step :: (Time m, HasCar ent) => ent -> m ent
step = mapMOf car LinearMotion.step

render :: (Renderer m) => Car -> m ()
render car' = drawCar (car' ^. position)
