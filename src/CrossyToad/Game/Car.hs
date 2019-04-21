{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Car
  ( Car(..)
  , HasCar(..)
  , mk
  ) where

import           Control.Lens
import           Linear.V2

import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))

data Car = Car
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __physical :: !Physical
  , __sprite :: !Sprite
  } deriving (Eq, Show)

makeClassy ''Car

instance HasPosition Car where
  position = _position

instance HasDirection Car where
  direction = _direction

instance HasLinearMotion Car where
  linearMotion = _linearMotion

instance HasPhysical Car where
  physical = _physical

instance HasSprite Car where
  sprite = _sprite

mk :: Position -> Direction -> Speed -> Car
mk pos dir carSpeed = Car
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk carSpeed
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
    , __sprite = Sprite ImageAsset.Car (V2 64 64)
    }
