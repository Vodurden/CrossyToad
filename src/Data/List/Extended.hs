module Data.List.Extended
  ( module Data.List
  , singleton
  ) where

import Data.List

-- | Wrap a single element into a list
singleton :: a -> [a]
singleton = pure
