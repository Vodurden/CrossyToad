{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Toad where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Physics.CollisionBox (CollisionBox(..), HasCollisionBox(..))
import qualified CrossyToad.Physics.CollisionBox as CollisionBox
import           CrossyToad.Physics.JumpMotion (JumpMotion(..), HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Effect.Renderer.Renderer
import           CrossyToad.Effect.Time.Time

data Toad = Toad
  { __position :: !Position
  , __jumpMotion :: !JumpMotion
  , __collisionBox :: !CollisionBox

  , _initialPosition :: !Position        -- ^ The position the toad was originally created in
  , _lives :: !Int                       -- ^ How many lives the toad has left
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasPosition Toad where
  position = _position

instance HasJumpMotion Toad where
  jumpMotion = _jumpMotion

instance HasCollisionBox Toad where
  collisionBox = _collisionBox

mk :: Position -> Toad
mk pos = Toad
    { __position = pos
    , __jumpMotion = JumpMotion.mk North toadSpeed toadDistance toadCooldown
    , __collisionBox = CollisionBox.mkOffset (V2 1 1) (V2 62 62)
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
      where secondsToJump = 0.10

    -- | How long the toad must rest between jumps
    toadCooldown :: Seconds
    toadCooldown = 0.15

step :: (Time m, HasToad ent) => ent -> m ent
step = mapMOf toad JumpMotion.step

render :: (Renderer m) => Toad -> m ()
render toad' | JumpMotion.isMoving toad'  = drawToad2 (toad' ^. position)
             | otherwise = drawToad (toad' ^. position)

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir = over (toad.jumpMotion) $ (JumpMotion.jump dir)

-- | Kills the toad
die :: Toad -> Toad
die toad' = toad' & (lives .~ max 0 (toad' ^. lives - 1))
                  . (position .~ toad' ^. initialPosition)

-- | Returns true if the toad is colliding with the entity
-- |
-- | A toad cannot collide with an entity if it is jumping.
collision :: (HasPosition ent, HasCollisionBox ent) => Toad -> ent -> Bool
collision toad' ent' =
  (CollisionBox.entCollision toad' ent')
  && (not $ JumpMotion.isMoving toad')
