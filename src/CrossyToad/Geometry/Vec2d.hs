module CrossyToad.Geometry.Vec2d where

import CrossyToad.Geometry.Internal

offset :: Offset u -> Vec2d u -> Vec2d u
offset offset' x = x + offset'
