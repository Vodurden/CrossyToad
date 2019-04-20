module CrossyToad.Renderer.Asset.Sprite.ToadHome
  ( Animation(..)
  , animations
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Linear.V2

import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Renderer.AnimationFrame (AnimationFrame)
import qualified CrossyToad.Renderer.AnimationFrame as AnimationFrame

data Animation
  = Empty
  | Filled
  deriving (Eq, Show, Ord)

mkFrame :: (V2 Int) -> Seconds -> AnimationFrame
mkFrame = AnimationFrame.mkIndexed (V2 128 128)

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