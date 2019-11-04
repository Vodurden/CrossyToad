module CrossyToad.Scene.MonadScene.IO.SceneCommand where

import CrossyToad.Scene.SceneId

data SceneCommand
  = Push SceneId
  | Pop
  | Clear
  deriving (Eq, Show)
