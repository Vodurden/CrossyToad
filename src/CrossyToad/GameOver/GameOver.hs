module CrossyToad.GameOver.GameOver
  ( scene
  ) where

import           Control.Lens
import           Data.Foldable (foldlM)
-- import           Data.Text (Text)
import           Linear.V2

import           CrossyToad.Input.Intent (Intent(..))
import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Input.Intents as Intents
import           CrossyToad.Physics.Direction (Direction(..))
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import           CrossyToad.Scene.MonadScene (MonadScene)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.SceneId as SceneId
import           CrossyToad.GameOver.GameOverState (HasGameOverState(..))
import qualified CrossyToad.GameOver.GameOverState as GameOverState

scene :: (MonadScene m, MonadRenderer m) => Int -> Scene m
scene totalScore' = Scene.mkNoTick (GameOverState.mk totalScore') handleInput render

handleInput :: forall m state. (MonadScene m, HasGameOverState state) => Intents -> state -> m state
handleInput intents state' =
    foldlM (flip applyIntent) state' (Intents.once intents)
  where
    applyIntent :: Intent -> state -> m state
    applyIntent EnterOrConfirm state = saveScore >> pure state
    applyIntent PauseOrExit state = exitWithoutSaving >> pure state
    applyIntent (Move dir) s = pure $ selectionChange dir s
    applyIntent _ state = pure state

    saveScore :: m ()
    saveScore = MonadScene.transition SceneId.Title

    exitWithoutSaving :: m ()
    exitWithoutSaving = MonadScene.transition SceneId.Title

    selectionChange :: Direction -> state -> state
    selectionChange North = gameOverState %~ GameOverState.letterUp
    selectionChange South = gameOverState %~ GameOverState.letterDown
    selectionChange East = gameOverState %~ GameOverState.previousLetter
    selectionChange West = gameOverState %~ GameOverState.nextLetter

render :: (MonadRenderer m) => state -> m ()
render _ = do
  MonadRenderer.clearScreen
  renderBanner
  -- renderName (GameOverState.currentName $ state' ^. gameOverState)
  MonadRenderer.drawScreen

renderBanner :: (MonadRenderer m) => m ()
renderBanner =
    MonadRenderer.drawText
      FontAsset.Title
      Nothing
      Nothing
      (Just titleClip)
      RGBAColour.white
      " GAME OVER "
  where
    titleClip = Clip.shrinkParallel shrinkX shrinkY $ Clip.mkAt titlePos titleSize
    shrinkX = (1280.0 / 10.0)
    shrinkY = (titleSize ^. _y) / 4.0
    titlePos = V2 0 0
    titleSize = V2 1280 (960 / 2.0)

-- renderName :: (MonadRenderer m) => Text -> m ()
-- renderName = undefined
