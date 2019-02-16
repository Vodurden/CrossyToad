{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Scene.MonadScene.IO.Env where

import Control.Lens
import Data.IORef

import CrossyToad.Scene.Scene
import CrossyToad.Scene.MonadScene.IO.SceneCommand

data Env m = Env
  { _scenesRef :: IORef ([Scene m])
  , _sceneCommandsRef :: IORef ([SceneCommand])
  }

makeClassy ''Env

initialize :: Scene m -> IO (Env m)
initialize scene' = Env <$> (newIORef [scene']) <*> (newIORef [])
