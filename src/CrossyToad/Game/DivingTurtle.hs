{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.DivingTurtle
  ( DivingTurtle(..)
  , HasDivingTurtle(..)
  , mk
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Physics.Direction (Direction, HasDirection(..))
import           CrossyToad.Physics.LinearMotion (LinearMotion, HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.Speed (Speed)
import           CrossyToad.Physics.Submersible (Submersible, HasSubmersible(..))
import qualified CrossyToad.Physics.Submersible as Submersible
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import           CrossyToad.Renderer.Asset.Animation.Turtle as TurtleAnimation

data DivingTurtle = DivingTurtle
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __physical :: !Physical
  , __submersible :: !Submersible
  , __animated :: !(Animated TurtleAnimation.Animation)
  } deriving (Eq, Show)

makeClassy ''DivingTurtle

instance HasPosition DivingTurtle where position = _position
instance HasDirection DivingTurtle where direction = _direction
instance HasLinearMotion DivingTurtle where linearMotion = _linearMotion
instance HasPhysical DivingTurtle where physical = _physical
instance HasSubmersible DivingTurtle where submersible = _submersible
instance HasAnimated DivingTurtle TurtleAnimation.Animation where animated = _animated

mk :: Position -> Direction -> Speed -> DivingTurtle
mk pos dir speed' = DivingTurtle
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk speed'
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Platform
    , __submersible = Submersible.mk 3 1
    , __animated = Animated.mk TurtleAnimation.Swimming TurtleAnimation.asset
    }
