module CrossyToad.Geometry.Offset where

import Linear.V2

-- | An offset applied to an object, measured in pixels
-- |
-- | An offset is basically a position except that it is meaningless
-- | on it's own and must be applied to a Position or Dimension
-- | to have any meaningful location
-- |
-- | In reality it's just a handy name for one use of a vector
type Offset = (V2 Float)
