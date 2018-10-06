module CrossyToad.Effect.SDLRenderer where

import           Control.Monad.IO.Class (MonadIO)
import           Foreign.C.Types (CInt)
import qualified SDL

class Monad m => SDLRenderer m where
  presentRenderer :: SDL.Renderer -> m ()
  clearRenderer :: SDL.Renderer -> m ()
  queryTexture :: SDL.Texture -> m SDL.TextureInfo
  drawTexture :: SDL.Renderer
              -> SDL.Texture
              -> Maybe (SDL.Rectangle CInt)
              -> Maybe (SDL.Rectangle CInt)
              -> m ()

presentRenderer' :: MonadIO m => SDL.Renderer -> m ()
presentRenderer' = SDL.present

clearRenderer' :: MonadIO m => SDL.Renderer -> m ()
clearRenderer' = SDL.clear

queryTexture' :: MonadIO m => SDL.Texture -> m SDL.TextureInfo
queryTexture' = SDL.queryTexture

drawTexture' :: MonadIO m
             => SDL.Renderer
             -> SDL.Texture
             -> Maybe (SDL.Rectangle CInt)
             -> Maybe (SDL.Rectangle CInt)
             -> m ()
drawTexture' renderer texture mSource mTarget = SDL.copy renderer texture mSource mTarget
