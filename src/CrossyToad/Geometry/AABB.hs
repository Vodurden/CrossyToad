{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Geometry.AABB where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Internal
import qualified CrossyToad.Geometry.Vec2d as Vec2d

makeClassyFor "HasAABB" "aabb"
  [ ("_topLeft", "topLeft")
  , ("_bottomRight", "bottomRight")
  ]
  ''AABB

-- makeClassy ''AABB
-- makeClassyFor "HasFoo" "foo" [("_foo", "fooLens"), ("bar", "lbar")] ''Foo

-- | Make an AABB with the given dimensions
mk :: Dimensions u -> AABB u
mk = mkOffset (V2 0 0)

-- | Make an AABB at the given offset with the given dimensions
mkOffset :: Offset u -> Dimensions u -> AABB u
mkOffset offset' dimensions' = AABB
  { _topLeft = offset'
  , _bottomRight = offset' + dimensions'
  }

-- | Synonym for the top-left of the AABB
minPoint :: Lens' (AABB a) (Position a)
minPoint = topLeft

-- | Synonym for the bottom-right of the AABB
maxPoint :: Lens' (AABB a) (Position a)
maxPoint = bottomRight

offset :: Offset u -> AABB u -> AABB u
offset offsetV = (topLeft %~ Vec2d.offset offsetV)
               . (bottomRight %~ Vec2d.offset offsetV)

overlapping :: AABB u -> AABB u -> Bool
overlapping aabb1 aabb2 =
  (aabb1^.maxPoint._x > aabb2^.minPoint._x)
  && (aabb1^.minPoint._x < aabb2^.maxPoint._x)
  && (aabb1^.maxPoint._y > aabb2^.minPoint._y)
  && (aabb1^.minPoint._y < aabb2^.maxPoint._y)
