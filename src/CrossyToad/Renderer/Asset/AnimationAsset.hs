{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Renderer.Asset.AnimationAsset
  ( AnimationAsset(..)
  , HasAnimationAsset(..)
  ) where

import Control.Lens
import Data.Map.Strict (Map)

import CrossyToad.Renderer.AnimationFrame (AnimationFrame)
import CrossyToad.Renderer.Asset.ImageAsset (ImageAsset, HasImageAsset(..))

data AnimationAsset key = AnimationAsset
  { __imageAsset :: !ImageAsset
  , _frames :: !(Map key [AnimationFrame])
  } deriving (Eq, Show)

makeClassy ''AnimationAsset

instance HasImageAsset (AnimationAsset key) where imageAsset = _imageAsset
