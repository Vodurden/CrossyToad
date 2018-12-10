{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Geometry.Size where

import Control.Lens
import Linear.V2

-- | The size of an object, measured in pixels
-- |
-- | This could represent
-- |
-- | - This width & height of an object on the screen
-- | - A region in a texture
-- | - The collision box of an entity in the game world
type Size = (V2 Int)

class HasSize a where
  size :: Lens' a Size

instance HasSize Size where
  size = id
