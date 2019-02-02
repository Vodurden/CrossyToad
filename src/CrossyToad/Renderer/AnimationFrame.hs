{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.AnimationFrame
  ( AnimationFrame(..)
  , HasAnimationFrame(..)
  , mk
  , mkIndexed
  ) where

import Control.Lens
import Linear.V2

import           CrossyToad.Renderer.Clip (Clip, HasClip(..))
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Time.Seconds
import           CrossyToad.Time.Timer (Timer, HasTimer(..))
import qualified CrossyToad.Time.Timer as Timer
import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Size

-- | Represents a single frame of an animation.
-- |
-- | Essentially this is what part of the spritesheet
-- | to render and how long to render it for.
data AnimationFrame = AnimationFrame
  { __clip :: Clip
  , __timer :: Timer
  } deriving (Eq, Show)

makeClassy ''AnimationFrame

instance HasClip AnimationFrame where
  clip = _clip

instance HasTimer AnimationFrame where
  timer = _timer

mk :: Position -> Size -> Seconds -> AnimationFrame
mk pos size' seconds' = AnimationFrame
  { __clip = Clip.mkAt pos size'
  , __timer = Timer.mk seconds'
  }

-- | Create an AnimationFrame based on a spritesheet index.
-- |
-- | This function assumes that all frames are the same size
-- | and that they are linearly distributed
mkIndexed :: Size -> (V2 Int) -> Seconds -> AnimationFrame
mkIndexed size' index' seconds' =
  mk ((fromIntegral <$> index') * size') size' seconds'
