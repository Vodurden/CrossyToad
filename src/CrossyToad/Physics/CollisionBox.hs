{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.CollisionBox
  ( CollisionBox(..)
  , HasCollisionBox(..)
  , mk
  , mkOffset
  , offset
  , collision
  , entCollision
  ) where

import Control.Lens
import Linear.V2

import CrossyToad.Physics.Position

data CollisionBox = CollisionBox
  { _minPoint :: Position
  , _maxPoint :: Position
  } deriving (Eq, Show)

makeClassy ''CollisionBox

mk :: (V2 Float) -> CollisionBox
mk wh = mkOffset (V2 0 0) wh

mkOffset :: (V2 Float) -> (V2 Float) -> CollisionBox
mkOffset (V2 x y) (V2 width height) = CollisionBox
  { _minPoint = V2 x y
  , _maxPoint = V2 (x+width) (y+height)
  }

-- | Shifts the collision box by a given offset
offset :: V2 Float -> CollisionBox -> CollisionBox
offset v = (minPoint +~ v) . (maxPoint +~ v)

-- | Returns true if there is a collision between these two entities
entCollision ::
  ( HasPosition e1
  , HasCollisionBox e1
  , HasPosition e2
  , HasCollisionBox e2
  ) => e1 -> e2 -> Bool
entCollision e1 e2 =
  let box1 = offset (e1 ^. position) (e1^.collisionBox)
      box2 = offset (e2 ^. position) (e2^.collisionBox)
  in collision box1 box2

-- | Returns true if there is a collision between the collision boxes
collision :: CollisionBox -> CollisionBox -> Bool
collision box1 box2 =
  (box1^.maxPoint > box2^.minPoint) && (box1^.minPoint < box2^.maxPoint)
