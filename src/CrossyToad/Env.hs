{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Env where

import           Control.Lens

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Effect.Input.SDL.SDL as SDLInput
import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Effect.Time.SDL.SDL as SDLTime

data Env = Env
  { _sceneEnv :: !Scene.Env
  , _sdlInputEnv :: !SDLInput.Env
  , _sdlRendererEnv :: !SDLRenderer.Env
  , _sdlTimeEnv :: !SDLTime.Env
  }

makeClassy ''Env

instance Scene.HasEnv Env where
  env = sceneEnv

instance SDLInput.HasEnv Env where
  env = sdlInputEnv

instance SDLRenderer.HasEnv Env where
  env = sdlRendererEnv

instance SDLTime.HasEnv Env where
  env = sdlTimeEnv
