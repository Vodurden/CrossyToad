module CrossyToad.Renderer.Asset.Animation.WoodLog
  ( Animation(..)
  , asset
  , floatLeftFromWidth
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Linear.V2

import           CrossyToad.Renderer.AnimationFrame (AnimationFrame)
import qualified CrossyToad.Renderer.AnimationFrame as AnimationFrame
import           CrossyToad.Renderer.Asset.AnimationAsset (AnimationAsset(..))
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset

data Animation
  = FloatLeftLen1
  | FloatLeftLen2
  | FloatLeftLen3
  | FloatLeftLen4
  | FloatLeftLen5
  | FloatLeftLen6
  deriving (Eq, Show, Ord)

asset :: AnimationAsset Animation
asset = AnimationAsset ImageAsset.WoodLog animations

animations :: Map Animation [AnimationFrame]
animations = Map.fromList
  [ (FloatLeftLen1, [AnimationFrame.mk (V2 0 (64 * 0)) (V2 (64 * 1) 64) 0.1])
  , (FloatLeftLen2, [AnimationFrame.mk (V2 0 (64 * 1)) (V2 (64 * 2) 64) 0.1])
  , (FloatLeftLen3, [AnimationFrame.mk (V2 0 (64 * 2)) (V2 (64 * 3) 64) 0.1])
  , (FloatLeftLen4, [AnimationFrame.mk (V2 0 (64 * 3)) (V2 (64 * 4) 64) 0.1])
  , (FloatLeftLen5, [AnimationFrame.mk (V2 0 (64 * 4)) (V2 (64 * 5) 64) 0.1])
  , (FloatLeftLen6, [AnimationFrame.mk (V2 0 (64 * 5)) (V2 (64 * 6) 64) 0.1])
  ]

floatLeftFromWidth :: Int -> Animation
floatLeftFromWidth 1 = FloatLeftLen1
floatLeftFromWidth 2 = FloatLeftLen2
floatLeftFromWidth 3 = FloatLeftLen3
floatLeftFromWidth 4 = FloatLeftLen4
floatLeftFromWidth 5 = FloatLeftLen5
floatLeftFromWidth 6 = FloatLeftLen6
floatLeftFromWidth _ = FloatLeftLen1
