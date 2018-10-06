module CrossyToad.Scene.Title where

import CrossyToad.Effect.Renderer

stepTitle :: (Renderer m) => m ()
stepTitle = drawTitleText (50, 140)
