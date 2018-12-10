{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Geometry.AABB where

import Control.Lens
import Linear.V2

import CrossyToad.Geometry.Position
import CrossyToad.Geometry.Size
import CrossyToad.Geometry.Offset

data AABB = AABB
  { _minPoint :: !Position
  , _maxPoint :: !Position
  } deriving (Eq, Show)

makeClassy ''AABB

instance HasPosition AABB where
  position = minPoint

_size :: Lens' AABB Size
_size = lens getter setter
  where
    getter :: AABB -> Size
    getter (AABB minP maxP) =  maxP - minP

    setter :: AABB -> Size -> AABB
    setter aabb' size' = aabb' & maxPoint .~ (aabb' ^. minPoint + size')

instance HasSize AABB where
  size = _size

mk :: Size -> AABB
mk wh = mkAt (V2 0 0) wh

mkAt :: Position -> Size -> AABB
mkAt pos dimensions' = AABB
  { _minPoint = pos
  , _maxPoint = pos + dimensions'
  }

-- | Shifts the collision box by a given offset
offset :: Offset -> AABB -> AABB
offset v = (minPoint +~ v) . (maxPoint +~ v)

-- | Returns true if there is a collision between the collision boxes
collision :: AABB -> AABB -> Bool
collision box1 box2 =
  (box1^.maxPoint._x > box2^.minPoint._x)
  && (box1^.minPoint._x < box2^.maxPoint._x)
  && (box1^.maxPoint._y > box2^.minPoint._y)
  && (box1^.minPoint._y < box2^.maxPoint._y)
