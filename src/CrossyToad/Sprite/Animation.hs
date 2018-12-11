{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CrossyToad.Sprite.Animation where

import           Control.Lens
import           Control.Zipper.Extended

import           CrossyToad.Sprite.AnimationFrame (AnimationFrame)

-- | An animation describes the set of frames to apply to
-- | an image asset representing a single concrete animation
-- |
-- | For example, the set of frames that describe the toads
-- | jump animation would be a single "Animation"
type Animation = Top :>> [AnimationFrame] :>> AnimationFrame

-- TODO: Consider whether these instances are lawful and whether
--       we can/should avoid orphan instances.
--
--       Also consider removing Show entirely in favor of Pretty
instance Eq Animation where
  (==) a b = a^.focus == b^.focus

instance Show Animation where
  show a = show $ a^.focus
