{-# LANGUAGE TypeApplications #-}

module CrossyToad.Scene.MonadScene.IO.MonadScene
  ( tickCurrentScene
  , getCurrentScene
  , delayPush
  , delayPop
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (modifyIORef', readIORef, writeIORef)
import           Data.Maybe
import           Data.List.Extended (foldl')

import           CrossyToad.Input.MonadInput (MonadInput)
import           CrossyToad.Logger.MonadLogger (MonadLogger)
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import           CrossyToad.Scene.MonadScene (MonadScene)
import           CrossyToad.Scene.MonadScene.IO.Env
import           CrossyToad.Scene.MonadScene.IO.SceneCommand (SceneCommand)
import qualified CrossyToad.Scene.MonadScene.IO.SceneCommand as SceneCommand
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Scene.SceneId (SceneId)
import qualified CrossyToad.Scene.SceneMapping as SceneMapping
import           CrossyToad.Time.MonadTime (MonadTime)

tickCurrentScene :: forall r m.
  ( MonadReader r m
  , HasEnv r m
  , MonadRenderer m
  , MonadScene m
  , MonadInput m
  , MonadLogger m
  , MonadTime m
  , MonadIO m
  ) => m (Maybe (Scene m))
tickCurrentScene = do
    scenesRef' <- view (env.scenesRef)

    -- Update the top scene. If the scene calls any of the push/pop/replace functions these
    -- will be added to the sceneCommandRef
    scenes' <- liftIO $ readIORef scenesRef'
    newScenes <- case scenes' of
      [] -> pure []
      (currentScene : rest) -> do
        nextCurrentScene <- Scene.tick currentScene
        pure (nextCurrentScene : rest)

    -- Apply the scene commands accumulated from the previous step
    sceneCommandsRef' <- view (env.sceneCommandsRef)
    sceneCommands' <- liftIO $ readIORef sceneCommandsRef'
    let commandedScenes = foldl' applyCommand newScenes sceneCommands'

    liftIO $ writeIORef scenesRef' commandedScenes
    liftIO $ writeIORef sceneCommandsRef' []

    pure $ listToMaybe commandedScenes
  where
    applyCommand :: [Scene m] -> SceneCommand -> [Scene m]
    applyCommand scenes' (SceneCommand.Push sceneId') = (SceneMapping.fromId sceneId' : scenes')
    applyCommand scenes' SceneCommand.Pop = drop 1 scenes'

getCurrentScene :: (MonadReader r m, HasEnv r m, MonadIO m) => m (Maybe (Scene m))
getCurrentScene = do
  scenesRef' <- view (env.scenesRef)
  scenes' <- liftIO $ readIORef scenesRef'
  pure $ listToMaybe $ scenes'

delayCommand :: (MonadReader r m, HasEnv r m, MonadIO m) => SceneCommand -> m ()
delayCommand command' = do
  sceneCommandsRef' <- view (env.sceneCommandsRef)
  liftIO $ modifyIORef' sceneCommandsRef' (command' :)

delayPush :: (MonadReader r m, HasEnv r m, MonadIO m) => SceneId -> m ()
delayPush sceneId' = delayCommand (SceneCommand.Push sceneId')

delayPop :: (MonadReader r m, HasEnv r m, MonadIO m) => m ()
delayPop = delayCommand (SceneCommand.Pop)
