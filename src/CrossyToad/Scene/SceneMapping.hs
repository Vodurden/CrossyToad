module CrossyToad.Scene.SceneMapping where

import           CrossyToad.Input.MonadInput (MonadInput)
import           CrossyToad.Logger.MonadLogger (MonadLogger)
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Game.Game as Game
import           CrossyToad.Scene.MonadScene (MonadScene)
import           CrossyToad.Scene.Scene (Scene)
import           CrossyToad.Scene.SceneId (SceneId)
import qualified CrossyToad.Scene.SceneId as SceneId
import qualified CrossyToad.Title.Title as Title
import           CrossyToad.Time.MonadTime (MonadTime)

fromId ::
  ( MonadRenderer m
  , MonadScene m
  , MonadInput m
  , MonadLogger m
  , MonadTime m
  ) => SceneId -> Scene m
fromId SceneId.Title = Title.scene
fromId SceneId.Game = Game.scene
