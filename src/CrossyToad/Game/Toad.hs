{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Toad where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.JumpMotion (JumpMotion(..), HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import qualified CrossyToad.Renderer.Asset.Sprite.Toad as ToadSprite
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Mortality.Mortal (Mortal, HasMortal(..))
import qualified CrossyToad.Mortality.Mortal as Mortal

data Toad = Toad
  { __position :: !Position
  , __direction :: !Direction
  , __jumpMotion :: !JumpMotion
  , __physical :: !Physical
  , __sprite :: !Sprite
  , __animated :: !(Animated ToadSprite.Animation)
  , __mortal :: !Mortal
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasPosition Toad where position = _position
instance HasDirection Toad where direction = _direction
instance HasJumpMotion Toad where jumpMotion = _jumpMotion
instance HasPhysical Toad where physical = _physical
instance HasSprite Toad where sprite = _sprite
instance HasAnimated Toad ToadSprite.Animation where animated = _animated
instance HasMortal Toad where mortal = _mortal

mk :: Position -> Toad
mk pos = Toad
    { __position = pos
    , __direction = North
    , __jumpMotion = JumpMotion.mk toadSpeed toadDistance toadCooldown
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
    , __sprite = Sprite ImageAsset.Toad (V2 64 64)
    , __animated = Animated.mk ToadSprite.Idle ToadSprite.animations
    , __mortal = Mortal.mk 5 pos
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

tick :: (HasToad ent) => Seconds -> ent -> ent
tick seconds =
  toad %~ (tickAnimatedState >>> Animated.tick seconds)

tickAnimatedState :: Toad -> Toad
tickAnimatedState t =
  t & animated %~
    if | JumpMotion.isJumping t -> (Animated.transition Animated.play) ToadSprite.Jump
       | otherwise -> (Animated.transition Animated.pause) ToadSprite.Idle

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir = JumpMotion.jump dir
