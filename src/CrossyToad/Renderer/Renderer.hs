-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Renderer.Renderer where

import           Control.Lens
import           Control.Monad.Reader
import           Linear.V2
import           SDL (textureWidth, textureHeight)
import qualified SDL as SDL

import qualified CrossyToad.Assets as Assets
import           CrossyToad.Config (Config)
import qualified CrossyToad.Config as Config
import           CrossyToad.Renderer.SDLRenderer

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

  drawTitleText :: V2 Float -> m ()
  drawToad :: V2 Float -> m ()

clearScreen' :: (MonadReader Config m, SDLRenderer m) => m ()
clearScreen' = do
  renderer <- view Config.renderer
  clearRenderer renderer

drawScreen' :: (MonadReader Config m, SDLRenderer m) => m ()
drawScreen' = do
  renderer <- view Config.renderer
  presentRenderer renderer

drawTitleText' :: (MonadReader Config m, SDLRenderer m) => V2 Float -> m ()
drawTitleText' pos = drawTextureSprite (view Assets.titleSprite) pos

drawToad' :: (MonadReader Config m, SDLRenderer m) => V2 Float -> m ()
drawToad' pos = drawTextureSprite (view Assets.toad) pos

drawTextureSprite :: (MonadReader Config m, SDLRenderer m)
                  => (Config -> SDL.Texture)
                  -> V2 Float
                  -> m ()
drawTextureSprite getTexture (V2 x y) = do
  renderer <- view Config.renderer
  texture <- asks getTexture
  SDL.TextureInfo{textureWidth, textureHeight} <- queryTexture texture
  let dimensions = V2 textureWidth textureHeight
  drawTexture
    renderer
    texture
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (truncate x) (truncate y)) dimensions)
