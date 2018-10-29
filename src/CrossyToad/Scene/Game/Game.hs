module CrossyToad.Scene.Game.Game where

import           CrossyToad.Renderer.Renderer

stepGame :: (Renderer m) => m ()
stepGame = do
  drawToad (0,0)
