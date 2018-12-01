{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Env where

import           Control.Lens

import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.SDL.SDL as SDLTime

data Env = Env
  { _sdlRendererEnv :: SDLRenderer.Env
  , _sdlTimeEnv :: SDLTime.Env
  }

makeClassy ''Env

instance SDLRenderer.HasEnv Env where
  env = sdlRendererEnv

instance SDLTime.HasEnv Env where
  env = sdlTimeEnv
