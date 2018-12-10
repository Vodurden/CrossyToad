{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Geometry.Position where

import Control.Lens
import Linear.V2

-- | A position measured in pixels
-- |
-- | This could represent:
-- |
-- | - The position of an object on the screen
-- | - A pixel in a texture
-- | - The position of an entity in the game world
-- |   (currently the game world is measured in pixels)
-- |
type Position = (V2 Int)

class HasPosition t where
  position :: Lens' t Position

instance HasPosition Position where
  position = id