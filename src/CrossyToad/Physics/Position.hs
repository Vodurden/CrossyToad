{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Position where

import Control.Lens
import Linear.V2

-- | The position of an object on the screen.
-- |
-- | Coordinates range from (0,0) (the top left of the screen) to
-- | (SCREEN_WIDTH, SCREEN_HEIGHT) (the bottom left of the screen)
type Position = (V2 Float)

class HasPosition t where
  position :: Lens' t Position

instance HasPosition Position where
  position = id
