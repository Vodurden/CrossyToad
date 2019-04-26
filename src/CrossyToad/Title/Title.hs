module CrossyToad.Title.Title
  ( scene
  ) where

import           Data.Foldable (traverse_)

import           CrossyToad.Input.Intents (Intents)
import           CrossyToad.Input.Intent (Intent(..))
import qualified CrossyToad.Input.Intents as Intents
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import           CrossyToad.Scene.MonadScene (MonadScene)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.SceneId as SceneId

scene :: (MonadScene m, MonadRenderer m) => Scene m
scene = Scene.mkNoState handleInput render

handleInput :: (MonadScene m) => Intents -> m ()
handleInput intents = traverse_ applyIntent (Intents.once intents)

applyIntent :: (MonadScene m) => Intent -> m ()
applyIntent EnterOrConfirm = MonadScene.delayPush SceneId.Game
applyIntent PauseOrExit = MonadScene.delayPop
applyIntent _ = pure ()

render :: (MonadRenderer m) => m ()
render = do
  MonadRenderer.clearScreen
  MonadRenderer.drawText FontAsset.Title Nothing Nothing Nothing RGBAColour.white " CROSSY TOAD "
  MonadRenderer.drawScreen
