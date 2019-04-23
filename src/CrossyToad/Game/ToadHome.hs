{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.ToadHome where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Animation.ToadHome as ToadHomeAnimation
import           CrossyToad.Victory.Scorable (Scorable, HasScorable(..))
import qualified CrossyToad.Victory.Scorable as Scorable
import           CrossyToad.Victory.Goal (Goal, HasGoal(..))
import qualified CrossyToad.Victory.Goal as Goal

data ToadHome = ToadHome
  { __position :: !Position
  , __physical :: !Physical
  , __animated :: !(Animated ToadHomeAnimation.Animation)
  , __scorable :: !Scorable
  , __goal :: !Goal
  } deriving (Eq, Show)

makeClassy ''ToadHome

instance HasPosition ToadHome where position = _position
instance HasPhysical ToadHome where physical = _physical
instance HasAnimated ToadHome ToadHomeAnimation.Animation where animated = _animated
instance HasScorable ToadHome where scorable = _scorable
instance HasGoal ToadHome where goal = _goal

mk :: Position -> ToadHome
mk pos = ToadHome
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
  , __animated = Animated.mk ToadHomeAnimation.Empty ToadHomeAnimation.asset
  , __scorable = Scorable.mk 500
  , __goal = Goal.mk
  }
