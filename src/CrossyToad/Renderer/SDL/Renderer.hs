module CrossyToad.Renderer.SDL.Renderer where

import           Control.Lens
import           Control.Monad.Reader
import           Linear.V2
import qualified SDL

import           CrossyToad.Renderer.Asset (Asset)
import           CrossyToad.Renderer.SDL.Env
import           CrossyToad.Renderer.SDL.Textures (HasTextures(..))
import qualified CrossyToad.Renderer.SDL.Textures as Textures
import           CrossyToad.Renderer.SDL.Texture (Texture, HasTexture(..))

clearScreen :: (MonadReader r m, HasEnv r, MonadIO m) => m ()
clearScreen = view renderer >>= SDL.clear

drawScreen :: (MonadReader r m, HasEnv r, MonadIO m) => m ()
drawScreen = view renderer >>= SDL.present

draw :: (MonadReader r m, MonadIO m, HasEnv r) => Asset -> V2 Float -> m ()
draw asset' pos = do
  config' <- view env
  let texture' = Textures.fromAsset asset' (config' ^. textures)
  drawTextureSprite texture' pos

drawTextureSprite :: (MonadReader r m, MonadIO m, HasEnv r)
                  => Texture
                  -> V2 Float
                  -> m ()
drawTextureSprite texture' (V2 x y) = do
  renderer' <- view renderer
  let dimensions = V2 (texture' ^. width) (texture' ^. height)
  SDL.copy
    renderer'
    (texture' ^. sdlTexture)
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (truncate x) (truncate y)) dimensions)
