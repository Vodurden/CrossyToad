{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Env where

import           Control.Lens

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Effect.Task.IO.Env as IOTask
import qualified CrossyToad.Effect.Logger.IO.Env as IOLogger
import qualified CrossyToad.Effect.Input.SDL.SDL as SDLInput
import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Effect.Time.SDL.SDL as SDLTime

data Env m = Env
  { _sceneEnv :: !Scene.Env
  , _ioTaskEnv :: !(IOTask.Env m)
  , _ioLoggerEnv :: !IOLogger.Env
  , _sdlInputEnv :: !SDLInput.Env
  , _sdlRendererEnv :: !SDLRenderer.Env
  , _sdlTimeEnv :: !SDLTime.Env
  }

makeClassy ''Env

instance Scene.HasEnv (Env m) where env = sceneEnv
instance IOTask.HasEnv (Env m) m where env = ioTaskEnv
instance IOLogger.HasEnv (Env m) where env = ioLoggerEnv
instance SDLInput.HasEnv (Env m) where env = sdlInputEnv
instance SDLRenderer.HasEnv (Env m) where env = sdlRendererEnv
instance SDLTime.HasEnv (Env m) where env = sdlTimeEnv
