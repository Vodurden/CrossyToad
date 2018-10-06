module CrossyToad.Scene.Scene where

import CrossyToad.Effect.Renderer
import CrossyToad.Scene.Title (stepTitle)

data Scene
  = Title
  deriving (Show, Eq)

step :: (Renderer m) => Scene -> m ()
step Title = stepTitle
