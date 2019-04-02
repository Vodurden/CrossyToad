module CrossyToad.Title.Title
  ( scene
  ) where

import           Control.Lens
import           Data.Foldable (traverse_)

import           CrossyToad.Input.InputState (InputState, HasInputState(..))
import           CrossyToad.Input.MonadInput (MonadInput)
import qualified CrossyToad.Input.MonadInput as MonadInput
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import           CrossyToad.Scene.MonadScene (MonadScene)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.SceneId as SceneId
import           CrossyToad.Title.Intent (Intent(..))
import qualified CrossyToad.Title.Intent as Intent

scene :: (MonadScene m, MonadInput m, MonadRenderer m) => Scene m
scene = Scene.mk () (const tick) (const render)

tick :: (MonadScene m, MonadInput m) => m ()
tick = do
  inputState' <- MonadInput.getInputState
  handleInput inputState'

handleInput :: (MonadScene m) => InputState -> m ()
handleInput input = do
  let intents = Intent.fromInput (input^.inputEvents)
  traverse_ stepIntent intents

stepIntent :: (MonadScene m) => Intent -> m ()
stepIntent StartGame = MonadScene.delayPush SceneId.Game
stepIntent Quit = MonadScene.delayPop

render :: (MonadRenderer m) => m ()
render = do
  MonadRenderer.clearScreen
  MonadRenderer.drawText FontAsset.Title Nothing Nothing Nothing RGBAColour.white " CROSSY TOAD "
  MonadRenderer.drawScreen
