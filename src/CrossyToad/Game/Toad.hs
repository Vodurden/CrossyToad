{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Toad where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Mortality.Mortal (Mortal, HasMortal(..))
import qualified CrossyToad.Mortality.Mortal as Mortal
import           CrossyToad.Physics.Direction (Direction(..), HasDirection(..))
import           CrossyToad.Physics.Distance (Distance)
import           CrossyToad.Physics.JumpMotion (JumpMotion(..), HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.Speed (Speed)
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Animation.Toad as ToadAnimation
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Victory.Score (Score, HasScore(..))
import qualified CrossyToad.Victory.Score as Score

data Toad = Toad
  { __position :: !Position
  , __direction :: !Direction
  , __jumpMotion :: !JumpMotion
  , __physical :: !Physical
  , __animated :: !(Animated ToadAnimation.Animation)
  , __mortal :: !Mortal
  , __score :: !Score
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasPosition Toad where position = _position
instance HasDirection Toad where direction = _direction
instance HasJumpMotion Toad where jumpMotion = _jumpMotion
instance HasPhysical Toad where physical = _physical
instance HasAnimated Toad ToadAnimation.Animation where animated = _animated
instance HasMortal Toad where mortal = _mortal
instance HasScore Toad where score = _score

mk :: Position -> Toad
mk pos = Toad
    { __position = pos
    , __direction = North
    , __jumpMotion = JumpMotion.mk toadSpeed toadDistance toadCooldown
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
    , __animated = Animated.mk ToadAnimation.IdleUp ToadAnimation.asset
    , __mortal = Mortal.mk 5 pos
    , __score = Score.mk
    }
  where
    -- | How far the toad moves in one jump
    toadDistance :: Distance
    toadDistance = 64

    -- | How many pixels the toad moves per-second
    toadSpeed :: Speed
    toadSpeed = toadDistance * (1 / secondsToJump)
      where secondsToJump = 0.15

    -- | How long the toad must rest between jumps
    toadCooldown :: Seconds
    toadCooldown = 0.15

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir = JumpMotion.jump dir
