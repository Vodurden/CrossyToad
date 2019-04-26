module Data.Map.Strict.Extended
  ( module Data.Map.Strict
  , create
  ) where

import Data.Map.Strict

-- | Insert a new key and value in the map. If the key is already present in the
-- | map, the original map is returned.
create :: Ord k => k -> a -> Map k a -> Map k a
create = insertWith (\_ oldValue -> oldValue)
