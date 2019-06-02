module CrossyToad.Renderer.AnimationSystem
  ( tickToadAnimation
  , tickToadHomeAnimation
  , tickTurtleAnimation
  , tickCrocAnimation
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens

import           CrossyToad.Physics.Direction (HasDirection(..))
import qualified CrossyToad.Physics.Direction as Direction
import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Submersible (HasSubmersible(..))
import qualified CrossyToad.Physics.Submersible as Submersible
import           CrossyToad.Renderer.Animated (HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Animation.Croc as CrocAnimation
import qualified CrossyToad.Renderer.Asset.Animation.Toad as ToadAnimation
import qualified CrossyToad.Renderer.Asset.Animation.ToadHome as ToadHomeAnimation
import qualified CrossyToad.Renderer.Asset.Animation.Turtle as TurtleAnimation
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

tickTurtleAnimation ::
  ( HasSubmersible ent
  , HasAnimated ent TurtleAnimation.Animation
  ) => Seconds -> ent -> ent
tickTurtleAnimation seconds =
    transitionTurtleAnimation >>> (Animated.tick seconds)
  where
    transitionTurtleAnimation ent =
      ent & animated %~
        if | ent ^. (to Submersible.swimming) ->
             if | (Submersible.progress ent) > 0.7 -> (Animated.transition Animated.play) TurtleAnimation.Diving
                | otherwise -> (Animated.transition Animated.play) TurtleAnimation.Swimming
           | ent ^. (to Submersible.sunk) -> (Animated.transition Animated.play) TurtleAnimation.Sunk

tickCrocAnimation ::
  ( HasSubmersible ent
  , HasAnimated ent CrocAnimation.Animation
  ) => Seconds -> ent -> ent
tickCrocAnimation seconds =
    transitionCrocAnimation >>> (Animated.tick seconds)
  where
    transitionCrocAnimation ent =
      ent & animated %~
        if | ent ^. (to Submersible.swimming) ->
             (Animated.transition Animated.play) CrocAnimation.Swimming
           | ent ^. (to Submersible.sunk) ->
             (Animated.transition Animated.play) CrocAnimation.Chomping
