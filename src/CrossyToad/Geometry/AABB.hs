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

-- makeClassy ''AABB
makeClassyFor "HasAABB" "aabb"
  [ ("_minPoint", "minPoint")
  , ("_maxPoint", "maxPoint")
  ] ''AABB

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

-- | Shrink the box by an even horizontal amount and an
-- | even vertical amount
shrinkParallel :: Double -> Double -> AABB -> AABB
shrinkParallel leftRight topBottom = shrink leftRight leftRight topBottom topBottom

-- | Shrink the box by the given amounts on each axis.
shrink :: Double -> Double -> Double -> Double -> AABB -> AABB
shrink left right top bottom box =
    box & (minPoint %~ (+ minPad))
        & (maxPoint %~ (+ maxPad))
  where
    minPad = (V2 left top)
    maxPad = negate <$> (V2 right bottom)
