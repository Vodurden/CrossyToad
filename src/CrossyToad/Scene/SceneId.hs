module CrossyToad.Scene.SceneId where

data SceneId
  = Title
  | Game
  | GameOver Int
  deriving (Eq, Show)
