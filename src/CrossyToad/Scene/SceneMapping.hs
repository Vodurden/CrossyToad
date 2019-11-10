module CrossyToad.Scene.SceneMapping where

import qualified CrossyToad.Game.Game as Game
import qualified CrossyToad.GameOver.GameOver as GameOver
import           CrossyToad.Logger.MonadLogger (MonadLogger)
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import           CrossyToad.Scene.MonadScene (MonadScene)
import           CrossyToad.Scene.Scene (Scene)
import           CrossyToad.Scene.SceneId (SceneId)
import qualified CrossyToad.Scene.SceneId as SceneId
import           CrossyToad.Stage.MonadStage (MonadStage)
import           CrossyToad.Victory.MonadHighScore (MonadHighScore)
import qualified CrossyToad.Title.Title as Title

fromId ::
  ( MonadRenderer m
  , MonadScene m
  , MonadLogger m
  , MonadStage m
  , MonadHighScore m
  ) => SceneId -> m (Scene m)
fromId SceneId.Title = pure Title.scene
fromId SceneId.Game = Game.scene
fromId (SceneId.GameOver score) = pure $ GameOver.scene score
