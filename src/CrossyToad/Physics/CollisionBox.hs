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

import CrossyToad.Geometry.Position
import CrossyToad.Geometry.Size
import CrossyToad.Geometry.Offset

data CollisionBox = CollisionBox
  { _minPoint :: !Position
  , _maxPoint :: !Position
  } deriving (Eq, Show)

makeClassy ''CollisionBox

mk :: Size -> CollisionBox
mk wh = mkOffset (V2 0 0) wh

mkOffset :: Position -> Size -> CollisionBox
mkOffset pos dimensions' = CollisionBox
  { _minPoint = pos
  , _maxPoint = pos + dimensions'
  }

-- | Shifts the collision box by a given offset
offset :: Offset -> CollisionBox -> CollisionBox
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
  (box1^.maxPoint._x > box2^.minPoint._x) && (box1^.minPoint._x < box2^.maxPoint._x)
  && (box1^.maxPoint._y > box2^.minPoint._y) && (box1^.minPoint._y < box2^.maxPoint._y)
