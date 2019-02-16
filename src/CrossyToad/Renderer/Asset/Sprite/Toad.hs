module CrossyToad.Renderer.Asset.Sprite.Toad
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
  = Idle
  | Jump
  deriving (Eq, Show, Ord)

mkFrame :: (V2 Int) -> Seconds -> AnimationFrame
mkFrame = AnimationFrame.mkIndexed (V2 128 128)

animations :: Map Animation [AnimationFrame]
animations = Map.fromList
  [ (Idle, idleAnimation)
  , (Jump, jumpAnimation)
  ]

idleAnimation :: [AnimationFrame]
idleAnimation =
  [ mkFrame (V2 0 0) 0.1
  ]

jumpAnimation :: [AnimationFrame]
jumpAnimation =
  [ mkFrame (V2 1 0) 0.15
  ]