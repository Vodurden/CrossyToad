{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Env where

import           Control.Lens

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.SDL.SDL as SDLTime

data Env = Env
  { _sceneEnv :: !Scene.Env
  , _sdlRendererEnv :: !SDLRenderer.Env
  , _sdlTimeEnv :: !SDLTime.Env
  }

makeClassy ''Env

instance Scene.HasEnv Env where
  env = sceneEnv

instance SDLRenderer.HasEnv Env where
  env = sdlRendererEnv

instance SDLTime.HasEnv Env where
  env = sdlTimeEnv
