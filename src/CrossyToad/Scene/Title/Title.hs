module CrossyToad.Scene.Title.Title where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Data.Foldable (traverse_)

import           CrossyToad.Input.Input
import           CrossyToad.Effect.Renderer
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Title.Intent (Intent(..))
import qualified CrossyToad.Scene.Title.Intent as Intent

stepTitle :: (MonadState s m, HasScene s, Input m, Renderer m) => m ()
stepTitle = do
  input <- getInput
  traverse_ stepIntent (Intent.fromInput input)

  drawTitleText (50, 140)

stepIntent :: (MonadState s m, HasScene s) => Intent -> m ()
stepIntent StartGame = assign scene Scene.Play
stepIntent Quit = assign scene Scene.Quit
