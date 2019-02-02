{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Env where

import           Control.Lens

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Time.MonadTask.IO.Env as IOMonadTask
import qualified CrossyToad.Logger.MonadLogger.IO.Env as IOMonadLogger
import qualified CrossyToad.Input.MonadInput.SDL.Env as SDLMonadInput
import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.MonadTime.SDL.Env as SDLMonadTime

data Env m = Env
  { _sceneEnv :: !Scene.Env
  , _ioTaskEnv :: !(IOMonadTask.Env m)
  , _ioLoggerEnv :: !IOMonadLogger.Env
  , _sdlInputEnv :: !SDLMonadInput.Env
  , _sdlRendererEnv :: !SDLRenderer.Env
  , _sdlTimeEnv :: !SDLMonadTime.Env
  }

makeClassy ''Env

instance Scene.HasEnv (Env m) where env = sceneEnv
instance IOMonadTask.HasEnv (Env m) m where env = ioTaskEnv
instance IOMonadLogger.HasEnv (Env m) where env = ioLoggerEnv
instance SDLMonadInput.HasEnv (Env m) where env = sdlInputEnv
instance SDLRenderer.HasEnv (Env m) where env = sdlRendererEnv
instance SDLMonadTime.HasEnv (Env m) where env = sdlTimeEnv
