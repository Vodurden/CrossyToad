module CrossyToad.Scene.MonadScene where

import CrossyToad.Scene.Scene
import CrossyToad.Scene.SceneId

class Monad m => MonadScene m where
  -- | Ticks the current scene and applies any delayed actions.
  -- |
  -- | Returns the current scene (if any)
  tickCurrentScene :: m (Maybe (Scene m))

  -- | Returns the current scene (if any)
  getCurrentScene :: m (Maybe (Scene m))

  -- | Prepares a scene to be pushed at the end of the current or next tick.
  delayPush :: SceneId -> m ()

  -- | Prepares to pop the top scene at the end of the current or next tick.
  delayPop :: m ()

delayReplace :: (MonadScene m) => SceneId -> m ()
delayReplace sceneId' = delayPop >> (delayPush sceneId')