{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Env where

import           Control.Lens

import qualified CrossyToad.Scene.MonadScene.IO.Env as IOMonadScene
import qualified CrossyToad.Logger.MonadLogger.IO.Env as IOMonadLogger
import qualified CrossyToad.Input.MonadInput.SDL.Env as SDLMonadInput
import qualified CrossyToad.Renderer.MonadRenderer.SDL.Env as SDLMonadRenderer
import qualified CrossyToad.Time.MonadTime.SDL.Env as SDLMonadTime

data Env m = Env
  { _ioSceneEnv :: !(IOMonadScene.Env m)
  , _ioLoggerEnv :: !IOMonadLogger.Env
  , _sdlInputEnv :: !SDLMonadInput.Env
  , _sdlRendererEnv :: !SDLMonadRenderer.Env
  , _sdlTimeEnv :: !SDLMonadTime.Env
  }

makeClassy ''Env

instance IOMonadScene.HasEnv (Env m) m where env = ioSceneEnv
instance IOMonadLogger.HasEnv (Env m) where env = ioLoggerEnv
instance SDLMonadInput.HasEnv (Env m) where env = sdlInputEnv
instance SDLMonadRenderer.HasEnv (Env m) where env = sdlRendererEnv
instance SDLMonadTime.HasEnv (Env m) where env = sdlTimeEnv
