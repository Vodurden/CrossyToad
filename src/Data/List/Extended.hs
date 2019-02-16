module Data.List.Extended
  ( module Data.List
  , singleton
  , mapHead
  ) where

import Data.List

-- | Wrap a single element into a list
singleton :: a -> [a]
singleton = pure

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (a : as) = (f a : as)
