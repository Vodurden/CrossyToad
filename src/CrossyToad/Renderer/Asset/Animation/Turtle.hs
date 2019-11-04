module CrossyToad.Renderer.Asset.Animation.Turtle
  ( Animation(..)
  , asset
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Linear.V2

import           CrossyToad.Renderer.AnimationFrame (AnimationFrame)
import qualified CrossyToad.Renderer.AnimationFrame as AnimationFrame
import           CrossyToad.Renderer.Asset.AnimationAsset (AnimationAsset(..))
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Time.Seconds (Seconds)

data Animation
  = Swimming
  | Diving
  | Sunk
  deriving (Eq, Show, Ord)

asset :: AnimationAsset Animation
asset = AnimationAsset ImageAsset.Turtle animations

animations :: Map Animation [AnimationFrame]
animations = Map.fromList
  [ (Swimming, [mkFrame (V2 0 0) 0.1])
  , (Diving, [mkFrame (V2 1 0) 0.1])
  , (Sunk, [mkFrame (V2 2 0) 0.1])
  ]

mkFrame :: (V2 Int) -> Seconds -> AnimationFrame
mkFrame = AnimationFrame.mkIndexed (V2 64 64) (V2 64 64)
