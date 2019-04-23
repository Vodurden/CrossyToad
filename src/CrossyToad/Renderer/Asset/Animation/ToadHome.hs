module CrossyToad.Renderer.Asset.Animation.ToadHome
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
  = Empty
  | Filled
  deriving (Eq, Show, Ord)

asset :: AnimationAsset Animation
asset = AnimationAsset ImageAsset.ToadHome animations (V2 64 64)

animations :: Map Animation [AnimationFrame]
animations = Map.fromList
  [ (Empty, emptyAnimation)
  , (Filled, filledAnimation)
  ]

emptyAnimation :: [AnimationFrame]
emptyAnimation =
  [ mkFrame (V2 0 0) 0
  ]

filledAnimation :: [AnimationFrame]
filledAnimation =
  [ mkFrame (V2 1 0) 0
  ]

mkFrame :: (V2 Int) -> Seconds -> AnimationFrame
mkFrame = AnimationFrame.mkIndexed (V2 128 128)
