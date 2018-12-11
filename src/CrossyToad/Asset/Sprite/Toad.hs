module CrossyToad.Asset.Sprite.Toad
  ( Animation(..)
  , animations
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Linear.V2

import           CrossyToad.Effect.Time.Seconds (Seconds)
import           CrossyToad.Sprite.AnimationFrame (AnimationFrame)
import qualified CrossyToad.Sprite.AnimationFrame as AnimationFrame

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
  , mkFrame (V2 0 1) 0.1
  ]

jumpAnimation :: [AnimationFrame]
jumpAnimation =
  [ mkFrame (V2 0 0) 0.1
  , mkFrame (V2 0 1) 0.1
  ]
