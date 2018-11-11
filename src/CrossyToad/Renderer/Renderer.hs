-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Renderer.Renderer where

import Linear.V2

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

  drawTitleText :: V2 Float -> m ()
  drawToad :: V2 Float -> m ()
  drawCar :: V2 Float -> m ()
