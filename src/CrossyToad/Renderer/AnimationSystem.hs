module CrossyToad.Renderer.AnimationSystem
  ( tickToadSprite
  , tickToadHomeSprite
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens

import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Renderer.Animated (HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Sprite.Toad as ToadSprite
import qualified CrossyToad.Renderer.Asset.Sprite.ToadHome as ToadHomeSprite
import           CrossyToad.Victory.Goal (HasGoal(..))
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

tickToadHomeSprite ::
  ( HasGoal ent
  , HasAnimated ent ToadHomeSprite.Animation
  ) => Seconds -> ent -> ent
tickToadHomeSprite seconds =
    transitionToadHomeSprite >>> (Animated.tick seconds)
  where
    transitionToadHomeSprite ent =
      ent & animated %~
        if | ent^.goal.reached -> (Animated.transition Animated.play) ToadHomeSprite.Filled
           | otherwise -> (Animated.transition Animated.play) ToadHomeSprite.Empty
