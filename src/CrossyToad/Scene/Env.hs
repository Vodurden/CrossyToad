{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Env where

import Control.Lens
import Data.IORef

import CrossyToad.Scene.SceneState

data Env = Env
  { __sceneStateRef :: IORef SceneState
  }

makeClassy ''Env

instance HasSceneStateIORef Env where
  sceneStateRef = _sceneStateRef

initialize :: IO Env
initialize = Env <$> (newIORef initialSceneState)