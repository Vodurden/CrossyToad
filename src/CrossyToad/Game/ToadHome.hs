{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.ToadHome where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import qualified CrossyToad.Renderer.Asset.Sprite.ToadHome as ToadHomeSprite
import           CrossyToad.Victory.Scorable (Scorable, HasScorable(..))
import qualified CrossyToad.Victory.Scorable as Scorable
import           CrossyToad.Victory.Goal (Goal, HasGoal(..))
import qualified CrossyToad.Victory.Goal as Goal

data ToadHome = ToadHome
  { __position :: !Position
  , __physical :: !Physical
  , __sprite :: !Sprite
  , __animated :: !(Animated ToadHomeSprite.Animation)
  , __scorable :: !Scorable
  , __goal :: !Goal
  } deriving (Eq, Show)

makeClassy ''ToadHome

instance HasPosition ToadHome where position = _position
instance HasPhysical ToadHome where physical = _physical
instance HasSprite ToadHome where sprite = _sprite
instance HasAnimated ToadHome ToadHomeSprite.Animation where animated = _animated
instance HasScorable ToadHome where scorable = _scorable
instance HasGoal ToadHome where goal = _goal

mk :: Position -> ToadHome
mk pos = ToadHome
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
  , __sprite = Sprite ImageAsset.ToadHome (V2 64 64)
  , __animated = Animated.mk ToadHomeSprite.Empty ToadHomeSprite.animations
  , __scorable = Scorable.mk 500
  , __goal = Goal.mk
  }
