module Data.List.Extended
  ( singleton
  ) where

-- | Wrap a single element into a list
singleton :: a -> [a]
singleton = pure