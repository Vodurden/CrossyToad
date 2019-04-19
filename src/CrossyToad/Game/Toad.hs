{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Toad where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..), HasLayer(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.JumpMotion (JumpMotion(..), HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import qualified CrossyToad.Renderer.Asset.Sprite.Toad as ToadSprite
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import           CrossyToad.Time.Seconds
import           CrossyToad.Time.TickSeconds

data Toad = Toad
  { __position :: !Position
  , __direction :: !Direction
  , __jumpMotion :: !JumpMotion
  , __physical :: !Physical
  , __sprite :: !Sprite
  , __animated :: !(Animated ToadSprite.Animation)

  , _initialPosition :: !Position        -- ^ The position the toad was originally created in
  , _lives :: !Int                       -- ^ How many lives the toad has left
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasPosition Toad where position = _position
instance HasDirection Toad where direction = _direction
instance HasJumpMotion Toad where jumpMotion = _jumpMotion
instance HasPhysical Toad where physical = _physical
instance HasSprite Toad where sprite = _sprite
instance HasAnimated Toad ToadSprite.Animation where animated = _animated

mk :: Position -> Toad
mk pos = Toad
    { __position = pos
    , __direction = North
    , __jumpMotion = JumpMotion.mk toadSpeed toadDistance toadCooldown
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
    , __sprite = Sprite ImageAsset.Toad (V2 64 64)
    , __animated = Animated.mk ToadSprite.Idle ToadSprite.animations
    , _initialPosition = pos
    , _lives = 5
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

step :: (HasToad ent) => TickSeconds -> ent -> ent
step (TickSeconds seconds) =
  toad %~ (JumpMotion.stepBy seconds
           >>> stepPhysicalState
           >>> stepAnimatedState
           >>> Animated.stepBy seconds)

stepPhysicalState :: Toad -> Toad
stepPhysicalState t =
  t & physical . layer .~
    if | JumpMotion.isJumping t -> Physical.Air
       | otherwise -> Physical.Ground

stepAnimatedState :: Toad -> Toad
stepAnimatedState t =
  t & animated %~
    if | JumpMotion.isJumping t -> (Animated.transition Animated.play) ToadSprite.Jump
       | otherwise -> (Animated.transition Animated.pause) ToadSprite.Idle

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir = JumpMotion.jump dir

-- | Kills the toad
die :: Toad -> Toad
die toad' = toad' & (lives .~ max 0 (toad' ^. lives - 1))
                  . (position .~ toad' ^. initialPosition)
