module CrossyToad.Effect.Renderer where

import           Control.Lens
import           Control.Monad.Reader
import           Linear.V2
import           SDL (textureWidth, textureHeight)
import qualified SDL as SDL

import qualified CrossyToad.Assets as Assets
import           CrossyToad.Config (Config)
import qualified CrossyToad.Config as Config
import           CrossyToad.Effect.SDLRenderer

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

  drawTitleText :: (Int, Int) -> m ()

clearScreen' :: (MonadReader Config m, SDLRenderer m) => m ()
clearScreen' = do
  renderer <- view Config.renderer
  clearRenderer renderer

drawScreen' :: (MonadReader Config m, SDLRenderer m) => m ()
drawScreen' = do
  renderer <- view Config.renderer
  presentRenderer renderer

drawTitleText' :: (MonadReader Config m, SDLRenderer m) => (Int, Int) -> m ()
drawTitleText' pos = drawTextureSprite (view Assets.titleSprite) pos

drawTextureSprite :: (MonadReader Config m, SDLRenderer m)
                  => (Config -> SDL.Texture)
                  -> (Int, Int)
                  -> m ()
drawTextureSprite getTexture (x,y) = do
  renderer <- view Config.renderer
  texture <- asks getTexture
  SDL.TextureInfo{textureWidth, textureHeight} <- queryTexture texture
  let dimensions = V2 textureWidth textureHeight
  drawTexture
    renderer
    texture
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dimensions)
