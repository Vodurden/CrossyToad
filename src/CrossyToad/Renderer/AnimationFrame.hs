{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.AnimationFrame
  ( AnimationFrame(..)
  , HasAnimationFrame(..)
  , mk
  , mkScaled
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
  , __size :: Size
  , __timer :: Timer
  } deriving (Eq, Show)

makeClassy ''AnimationFrame

instance HasClip AnimationFrame where clip = _clip
instance HasSize AnimationFrame where size = _size
instance HasTimer AnimationFrame where timer = _timer

mk :: Position -> Size -> Seconds -> AnimationFrame
mk pos size' seconds' = mkScaled pos size' size' seconds'

-- | Create an AnimationFrame where the clip size and render size differ.
mkScaled :: Position -> Size -> Size -> Seconds -> AnimationFrame
mkScaled pos size' targetSize' seconds' = AnimationFrame
  { __clip = Clip.mkAt pos size'
  , __size = targetSize'
  , __timer = Timer.mk seconds'
  }

-- | Create an AnimationFrame based on a spritesheet index.
-- |
-- | This function assumes that all frames are the same size
-- | and that they are linearly distributed
mkIndexed :: Size -> Size -> (V2 Int) -> Seconds -> AnimationFrame
mkIndexed size' targetSize' index' seconds' =
  mkScaled ((fromIntegral <$> index') * size') size' targetSize' seconds'
