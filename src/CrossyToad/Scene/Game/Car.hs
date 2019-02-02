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

import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Renderer.RenderCommand (RenderCommand)
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.CollisionBox (CollisionBox, HasCollisionBox(..))
import qualified CrossyToad.Physics.CollisionBox as CollisionBox
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import qualified CrossyToad.Renderer.Sprite as Sprite
import           CrossyToad.Time.MonadTime

data Car = Car
  { __position :: !Position
  , __direction :: !Direction
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

instance HasDirection Car where
  direction = _direction

instance HasLinearMotion Car where
  linearMotion = _linearMotion

instance HasCollisionBox Car where
  collisionBox = _collisionBox

instance HasSprite Car where
  sprite = _sprite

mk :: Position -> Direction -> Car
mk pos dir = Car
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk carSpeed
    , __collisionBox = CollisionBox.mkAt (V2 1 1) (V2 62 62)
    , __sprite = Sprite ImageAsset.Car (V2 64 64)
    }
  where
    -- | How far the car moves in one second
    carSpeed :: Speed
    carSpeed = 64 * (1 / secondsPerTile)
      where secondsPerTile = 0.5

stepAll :: (MonadTime m, HasCars ent) => ent -> m ent
stepAll = (cars.traverse) step

step :: (MonadTime m, HasCar ent) => ent -> m ent
step = mapMOf car LinearMotion.step

render :: Car -> RenderCommand
render = Sprite.render
