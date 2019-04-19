{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CrossyToad.Renderer.Animation
  ( AnimationState(..)
  , Animation(..)
  , HasAnimation(..)
  , mk
  , tick
  , currentFrame
  , animate
  , play
  , loop
  , pause
  ) where

import           Control.Lens
import           Control.Zipper.Extended

import           CrossyToad.Renderer.AnimationFrame (AnimationFrame)
import           CrossyToad.Time.Seconds
import           CrossyToad.Time.Timer (HasTimer(..))
import qualified CrossyToad.Time.Timer as Timer

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

tick :: Seconds -> Animation -> Animation
tick delta anim
  | anim ^. state == Paused = anim
  | otherwise = tickFrameDelta delta anim

tickFrameDelta :: Seconds -> Animation -> Animation
tickFrameDelta delta anim =
  anim & Timer.tickOver (frames.focus) delta nextFrame

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
