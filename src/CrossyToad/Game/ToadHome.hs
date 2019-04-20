{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.ToadHome where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Victory.Scorable (Scorable, HasScorable(..))
import qualified CrossyToad.Victory.Scorable as Scorable

data ToadHome = ToadHome
  { __position :: !Position
  , __physical :: !Physical
  , __sprite :: !Sprite
  , __scorable :: !Scorable
  , _containsToad :: !Bool
  } deriving (Eq, Show)

makeClassy ''ToadHome

instance HasPosition ToadHome where position = _position
instance HasPhysical ToadHome where physical = _physical
instance HasSprite ToadHome where sprite = _sprite
instance HasScorable ToadHome where scorable = _scorable

mk :: Position -> ToadHome
mk pos = ToadHome
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Platform
  , __sprite = Sprite ImageAsset.Road (V2 64 64)
  , __scorable = Scorable.mk 500
  , _containsToad = False
  }
