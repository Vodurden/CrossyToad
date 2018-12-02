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

import           CrossyToad.Effect.Renderer.Renderer
import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Time.Time
import           CrossyToad.Physics.CollisionBox (CollisionBox(..), HasCollisionBox(..))
import qualified CrossyToad.Physics.CollisionBox as CollisionBox
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Sprite.Sprite (Sprite(..), HasSprite(..))
import qualified CrossyToad.Sprite.Sprite as Sprite

data Car = Car
  { __position :: !Position
  , __linearMotion :: !LinearMotion
  , __collisionBox :: !CollisionBox
  , __sprite :: !Sprite
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

instance HasSprite Car where
  sprite = _sprite

mk :: Position -> Direction -> Car
mk pos dir = Car
    { __position = pos
    , __linearMotion = LinearMotion.mk dir carSpeed
    , __collisionBox = CollisionBox.mkOffset (V2 1 1) (V2 62 62)
    , __sprite = Sprite ImageAsset.Car (V2 64 64)
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
render = Sprite.render
