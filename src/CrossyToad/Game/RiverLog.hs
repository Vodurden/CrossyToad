{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.RiverLog
  ( RiverLog(..)
  , HasRiverLog(..)
  , mk
  ) where

import           Control.Lens
import           Linear.V2

import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))

data RiverLog = RiverLog
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __sprite :: !Sprite
  , __physical :: !Physical
  } deriving (Eq, Show)

makeClassy ''RiverLog

instance HasPosition RiverLog where position = _position
instance HasDirection RiverLog where direction = _direction
instance HasLinearMotion RiverLog where linearMotion = _linearMotion
instance HasSprite RiverLog where sprite = _sprite
instance HasPhysical RiverLog where physical = _physical

mk :: Position -> Direction -> Speed -> RiverLog
mk pos dir logSpeed = RiverLog
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk logSpeed
    , __sprite = Sprite ImageAsset.Car (V2 64 64)
    , __physical = Physical.mkAt (V2 0 0) (V2 64 64) Physical.Platform
    }
