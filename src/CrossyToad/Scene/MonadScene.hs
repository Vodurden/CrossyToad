module CrossyToad.Scene.MonadScene where

import CrossyToad.Scene.Scene
import CrossyToad.Scene.SceneId
import CrossyToad.Time.TickSeconds (TickSeconds)

class Monad m => MonadScene m where
  -- | Ticks the current scene and applies any delayed actions.
  -- |
  -- | Returns the current scene (if any)
  tickCurrentScene :: TickSeconds -> m (Maybe (Scene m))

  -- | Displays the current scene
  renderCurrentScene :: m ()

  -- | Returns the current scene (if any)
  getCurrentScene :: m (Maybe (Scene m))

  -- | Prepares a scene to be pushed at the end of the current or next tick.
  delayPush :: SceneId -> m ()

  -- | Prepares to pop the top scene at the end of the current or next tick.
  delayPop :: m ()

delayReplace :: (MonadScene m) => SceneId -> m ()
delayReplace sceneId' = delayPop >> (delayPush sceneId')
