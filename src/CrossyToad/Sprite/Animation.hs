{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CrossyToad.Sprite.Animation
  ( AnimationState(..)
  , Animation(..)
  , HasAnimation(..)
  , mk
  , stepBy
  , currentFrame
  , animate
  , play
  , loop
  , pause
  ) where

import           Control.Lens
import           Control.Monad.State (execState)
import           Control.Zipper.Extended

import           CrossyToad.Effect.Time.Time
import           CrossyToad.Effect.Time.Timer (HasTimer(..))
import qualified CrossyToad.Effect.Time.Timer as Timer
import           CrossyToad.Sprite.AnimationFrame (AnimationFrame)

-- TODO: Consider whether these instances are lawful and whether
--       we can/should avoid orphan instances.
--
--       Also consider removing Show entirely in favor of Pretty
instance Eq (Top :>> [AnimationFrame] :>> AnimationFrame) where
  (==) a b = a^.focus == b^.focus

instance Show (Top :>> [AnimationFrame] :>> AnimationFrame) where
  show a = show $ a^.focus

data AnimationState
  = Paused
  | Playing
  | Looping
  deriving (Eq, Show)

-- | An animation describes the set of frames to apply to
-- | an image asset representing a single concrete animation
-- |
-- | For example, the set of frames that describe the toads
-- | jump animation would be a single "Animation"
data Animation = Animation
  { _frames :: Top :>> [AnimationFrame] :>> AnimationFrame
  , _state :: AnimationState
  } deriving (Eq, Show)

makeClassy ''Animation

mk :: [AnimationFrame] -> Animation
mk frames' = Animation
  { _frames = zipper frames' & fromWithin traverse
  , _state = Paused
  }

currentFrame :: Lens' Animation AnimationFrame
currentFrame = (frames . focus)

stepBy :: Seconds -> Animation -> Animation
stepBy delta anim
  | anim ^. state == Paused = anim
  | otherwise = stepFrameDelta delta anim

stepFrameDelta :: Seconds -> Animation -> Animation
stepFrameDelta delta anim =
  let nextFrame' = execState (Timer.stepBy delta) (anim^.frames.focus)
  in if Timer.finished (nextFrame' ^. timer)
     then nextFrame anim
     else anim

-- | Switches to the next frame.
nextFrame :: Animation -> Animation
nextFrame anim =
  case anim ^. state of
    Paused -> anim
    Playing -> anim & (frames %~ tug rightward)
                    . (frames . focus . timer %~ Timer.start)
    Looping -> anim & (frames %~ wrapRight)
                    . (frames . focus .timer %~ Timer.start)

-- | Switch to the first frame of the animation.
-- |
-- | This function retains the existing looping state
animate :: Animation -> Animation
animate = (frames %~ leftmost)
          . (frames . focus . timer %~ Timer.start)

-- | Switch to the first frame of the animation and plays it.
-- |
-- | The animation will be executed once and will
-- | stop on the last frame once finished
play :: Animation -> Animation
play = animate . (state .~ Playing)

-- | Switches to the first frame of the given animation and loops it
-- |
-- | The animation will be executed in a loop until another
-- | animation is selected
loop :: Animation -> Animation
loop = animate . (state .~ Looping)

-- | Switches to the first frame of the given animation and pauses
-- | on it.
pause :: Animation -> Animation
pause = animate . (state .~ Paused)
