module CrossyToad.GameOver.GameOver
  ( scene
  ) where

import           Control.Lens
import           Data.Foldable (foldlM)
import           Data.Text (Text)
import qualified Data.Text as Text

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
    selectionChange East = gameOverState %~ GameOverState.nextLetter
    selectionChange West = gameOverState %~ GameOverState.previousLetter

render :: (MonadRenderer m, HasGameOverState state) => state -> m ()
render state' = do
    MonadRenderer.clearScreen

    MonadRenderer.drawText FontAsset.Title Nothing Nothing (Just titleClip) RGBAColour.white title'
    MonadRenderer.drawText FontAsset.PressStart Nothing Nothing (Just nameClip) RGBAColour.white name'
    MonadRenderer.drawText FontAsset.PressStart Nothing Nothing (Just scoreClip) RGBAColour.white score'
    MonadRenderer.drawText FontAsset.PressStart Nothing Nothing (Just helpClip) RGBAColour.white help'

    MonadRenderer.drawScreen
  where
    -- Text
    title' = "GAME OVER" :: Text
    name' = GameOverState.currentName $ state' ^. gameOverState
    score' = Text.pack $ show $ state' ^. gameOverState . totalScore
    help' = "[Up/Down/Left/Right]:Edit Name    [Confirm]:Save Score    [Exit]:Quit" :: Text

    -- Layout
    titleClip = Clip.fromGrid 1 1 18 4
    nameClip = Clip.fromGrid 3 8 5 3
    scoreClip = Clip.fromGrid 11 8 5 3
    helpClip = Clip.fromGrid 0 14 20 1
