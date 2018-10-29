module CrossyToad.Scene.Game.Game where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Data.Foldable (traverse_)

import           CrossyToad.Input.Input
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Game.Intent (Intent(..))
import qualified CrossyToad.Scene.Game.Intent as Intent

stepGame :: (MonadState s m, HasScene s, Input m, Renderer m) => m ()
stepGame = do
  input <- getInput
  traverse_ stepIntent (Intent.fromInput input)

  drawToad (0,0)

stepIntent :: (MonadState s m, HasScene s) => Intent -> m ()
stepIntent move@Move{} = undefined
stepIntent Exit = assign scene Scene.Title
