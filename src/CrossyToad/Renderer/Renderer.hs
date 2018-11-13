-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Renderer.Renderer where

import           Linear.V2

import           CrossyToad.Renderer.Asset (Asset)
import qualified CrossyToad.Renderer.Asset as Asset

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

  draw :: Asset -> V2 Float -> m ()

drawTitleText :: (Renderer m) => V2 Float -> m ()
drawTitleText = draw Asset.TitleSprite

drawToad :: (Renderer m) => V2 Float -> m ()
drawToad = draw Asset.Toad

drawCar :: (Renderer m) => V2 Float -> m ()
drawCar = draw Asset.Car
