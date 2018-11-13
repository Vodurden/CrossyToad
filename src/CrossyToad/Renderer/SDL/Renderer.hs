module CrossyToad.Renderer.SDL.Renderer where

import           Control.Lens
import           Control.Monad.Reader
import           Linear.V2
import qualified SDL

import           CrossyToad.Renderer.Asset (Asset)
import           CrossyToad.Renderer.SDL.Config
import           CrossyToad.Renderer.SDL.Textures (HasTextures(..))
import qualified CrossyToad.Renderer.SDL.Textures as Textures

clearScreen :: (MonadReader r m, HasConfig r, MonadIO m) => m ()
clearScreen = view renderer >>= SDL.clear

drawScreen :: (MonadReader r m, HasConfig r, MonadIO m) => m ()
drawScreen = view renderer >>= SDL.present

draw :: (MonadReader r m, MonadIO m, HasConfig r) => Asset -> V2 Float -> m ()
draw asset' pos = do
  config' <- view config
  let texture' = Textures.fromAsset asset' (config' ^. textures)
  drawTextureSprite texture' pos

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
