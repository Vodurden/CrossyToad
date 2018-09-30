module CrossyFrog.Effect.Renderer where

import Control.Monad.Reader
import qualified SDL

import CrossyFrog.Config

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

clearScreen' :: (MonadReader Config m, MonadIO m) => m ()
clearScreen' = do
  renderer <- asks cRenderer
  SDL.clear renderer

drawScreen' :: (MonadReader Config m, MonadIO m) => m ()
drawScreen' = do
  renderer <- asks cRenderer
  SDL.present renderer
