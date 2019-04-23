module CrossyToad.Renderer.AnimationSystem
  ( tickToadAnimation
  , tickToadHomeAnimation
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens

import           CrossyToad.Physics.Direction (HasDirection(..))
import qualified CrossyToad.Physics.Direction as Direction
import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Renderer.Animated (HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Animation.Toad as ToadAnimation
import qualified CrossyToad.Renderer.Asset.Animation.ToadHome as ToadHomeAnimation
import           CrossyToad.Victory.Goal (HasGoal(..))
import           CrossyToad.Time.Seconds (Seconds)

tickToadAnimation :: forall ent.
  ( HasDirection ent
  , HasJumpMotion ent
  , HasAnimated ent ToadAnimation.Animation
  ) => Seconds -> ent -> ent
tickToadAnimation seconds =
    transitionToadAnimation >>> (Animated.tick seconds)
  where
    -- TODO: Deal with direction better here and in animations
    transitionToadAnimation ent =
      let nextAnimation =
              if | JumpMotion.isJumping ent ->
                  if | Direction.horizontal ent -> ToadAnimation.JumpLeft
                     | otherwise -> ToadAnimation.JumpUp
                 | otherwise ->
                  if | Direction.horizontal ent -> ToadAnimation.IdleLeft
                     | otherwise -> ToadAnimation.IdleUp
      in ent & animated %~ (Animated.transition Animated.play) nextAnimation

tickToadHomeAnimation ::
  ( HasGoal ent
  , HasAnimated ent ToadHomeAnimation.Animation
  ) => Seconds -> ent -> ent
tickToadHomeAnimation seconds =
    transitionToadHomeAnimation >>> (Animated.tick seconds)
  where
    transitionToadHomeAnimation ent =
      ent & animated %~
        if | ent^.goal.reached -> (Animated.transition Animated.play) ToadHomeAnimation.Filled
           | otherwise -> (Animated.transition Animated.play) ToadHomeAnimation.Empty
