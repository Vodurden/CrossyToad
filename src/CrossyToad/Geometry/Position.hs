{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Geometry.Position
  ( HasPosition(..)
  ) where

import Control.Lens

import CrossyToad.Geometry.Internal

class HasPosition s u where
  position :: Lens' s (Position u)

instance HasPosition (Position u) u where
  position = id
