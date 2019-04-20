module CrossyToad.Renderer.AnimationSystem
  ( tickToadSprite
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens

import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Renderer.Animated (HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Sprite.Toad as ToadSprite
import           CrossyToad.Time.Seconds (Seconds)

tickToadSprite ::
  ( HasJumpMotion ent
  , HasAnimated ent ToadSprite.Animation
  ) => Seconds -> ent -> ent
tickToadSprite seconds =
    transitionToadSprite >>> (Animated.tick seconds)
  where
    transitionToadSprite ent =
      ent & animated %~
        if | JumpMotion.isJumping ent -> (Animated.transition Animated.play) ToadSprite.Jump
           | otherwise -> (Animated.transition Animated.pause) ToadSprite.Idle
