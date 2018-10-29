module CrossyToad.Scene.Title.Title where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Data.Foldable (traverse_)
import           Linear.V2

import           CrossyToad.Input.Input
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Title.Intent (Intent(..))
import qualified CrossyToad.Scene.Title.Intent as Intent

stepTitle :: (MonadState s m, HasScene s, Input m, Renderer m) => m ()
stepTitle = do
  input <- getInput
  traverse_ stepIntent (Intent.fromInput input)

  drawTitleText $ V2 50 140

stepIntent :: (MonadState s m, HasScene s) => Intent -> m ()
stepIntent StartGame = assign scene Scene.Game
stepIntent Quit = assign scene Scene.Quit
