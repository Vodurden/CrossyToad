module CrossyToad.Renderer.Asset.Animation.Toad
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
  = IdleUp
  | IdleLeft
  | JumpUp
  | JumpLeft
  deriving (Eq, Show, Ord)

asset :: AnimationAsset Animation
asset = AnimationAsset ImageAsset.Toad animations

animations :: Map Animation [AnimationFrame]
animations = Map.fromList
  [ (IdleUp, [mkFrame (V2 0 0) 0.1])
  , (IdleLeft, [mkFrame (V2 0 1) 0.1])
  , (JumpUp, [mkFrame (V2 1 0) 0.15])
  , (JumpLeft, [mkFrame (V2 1 1) 0.15])
  ]

mkFrame :: (V2 Int) -> Seconds -> AnimationFrame
mkFrame = AnimationFrame.mkIndexed (V2 128 128) (V2 64 64)
