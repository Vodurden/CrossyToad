module CrossyToad.Renderer.SDL.Renderer where

import           Control.Lens
import           Control.Monad.Reader
import           Linear.V2
import qualified SDL

import           CrossyToad.Renderer.SDL.Config
import           CrossyToad.Renderer.SDL.Assets

clearScreen :: (MonadReader r m, HasConfig r, MonadIO m) => m ()
clearScreen = view renderer >>= SDL.clear

drawScreen :: (MonadReader r m, HasConfig r, MonadIO m) => m ()
drawScreen = view renderer >>= SDL.present

drawTitleText :: (MonadReader r m, MonadIO m, HasConfig r) => V2 Float -> m ()
drawTitleText pos = view (config.titleSprite) >>= flip drawTextureSprite pos

drawToad :: (MonadReader r m, MonadIO m, HasConfig r) => V2 Float -> m ()
drawToad pos = view (config.toad) >>= flip drawTextureSprite pos

drawTextureSprite :: (MonadReader r m, MonadIO m, HasConfig r)
                  => SDL.Texture
                  -> V2 Float
                  -> m ()
drawTextureSprite texture (V2 x y) = do
  renderer' <- view renderer
  SDL.TextureInfo { textureWidth, textureHeight } <- SDL.queryTexture texture
  let dimensions = V2 textureWidth textureHeight
  SDL.copy
    renderer'
    texture
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (truncate x) (truncate y)) dimensions)
