module Data.Bounded.Extended
  ( succWrap
  , predWrap
  ) where

-- | Equivalent to `Enum.succ` but wraps around if
-- | we try to succ past the maxBound
succWrap :: (Eq a, Enum a, Bounded a) => a -> a
succWrap a | a == maxBound = minBound
           | otherwise = succ a

-- | Equivalent to `Enum.pred` but wraps around if
-- | we try to pred past the minBound
predWrap :: (Eq a, Enum a, Bounded a) => a -> a
predWrap a | a == minBound = maxBound
           | otherwise = pred a
