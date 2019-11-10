{-# LANGUAGE TypeApplications #-}

module CrossyToad.Scene.MonadScene.IO.MonadScene
  ( tickCurrentScene
  , renderCurrentScene
  , handleInputCurrentScene
  , getCurrentScene
  , delayPush , delayPop
  , delayClear
  ) where

import           Control.Lens
import           Control.Monad (when, void)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef (modifyIORef', readIORef, writeIORef)
import           Data.Maybe
import           Data.Foldable (foldlM)

import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Logger.LogLevel as LogLevel
import           CrossyToad.Logger.MonadLogger (MonadLogger)
import qualified CrossyToad.Logger.MonadLogger as MonadLogger
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import           CrossyToad.Scene.MonadScene (MonadScene)
import           CrossyToad.Scene.MonadScene.IO.Env
import           CrossyToad.Scene.MonadScene.IO.SceneCommand (SceneCommand)
import qualified CrossyToad.Scene.MonadScene.IO.SceneCommand as SceneCommand
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Scene.SceneId (SceneId)
import qualified CrossyToad.Scene.SceneMapping as SceneMapping
import           CrossyToad.Stage.MonadStage (MonadStage)
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Victory.MonadHighScore (MonadHighScore)

handleInputCurrentScene :: forall r m.
  ( MonadReader r m
  , HasEnv r m
  , MonadScene m
  , MonadStage m
  , MonadRenderer m
  , MonadLogger m
  , MonadHighScore m
  , MonadIO m
  ) => Intents -> m (Maybe (Scene m))
handleInputCurrentScene intents =
  overCurrentScene $ \currentScene -> do
    Scene.handleInput intents currentScene

tickCurrentScene :: forall r m.
  ( MonadReader r m
  , HasEnv r m
  , MonadScene m
  , MonadStage m
  , MonadLogger m
  , MonadRenderer m
  , MonadHighScore m
  , MonadIO m
  ) => Seconds -> m (Maybe (Scene m))
tickCurrentScene seconds =
  overCurrentScene $ \currentScene -> Scene.tick seconds currentScene

overCurrentScene :: forall r m.
  ( MonadReader r m
  , HasEnv r m
  , MonadScene m
  , MonadStage m
  , MonadRenderer m
  , MonadLogger m
  , MonadHighScore m
  , MonadIO m
  ) => (Scene m -> m (Scene m)) -> m (Maybe (Scene m))
overCurrentScene f = do
    scenesRef' <- view (env.scenesRef)

    -- Update the top scene. If the scene calls any of the push/pop/replace functions these
    -- will be added to the sceneCommandRef
    scenes' <- liftIO $ readIORef scenesRef'
    newScenes <- case scenes' of
      [] -> pure []
      (currentScene : rest) -> do
        nextCurrentScene <- f currentScene
        pure (nextCurrentScene : rest)

    -- Apply the scene commands accumulated from the previous step
    sceneCommandsRef' <- view (env.sceneCommandsRef)
    sceneCommands' <- liftIO $ readIORef sceneCommandsRef'
    when (sceneCommands' /= []) $ MonadLogger.logShow LogLevel.Debug sceneCommands'
    commandedScenes <- foldlM applyCommand newScenes sceneCommands'

    liftIO $ writeIORef scenesRef' commandedScenes
    liftIO $ writeIORef sceneCommandsRef' []

    pure $ listToMaybe commandedScenes
  where
    applyCommand :: [Scene m] -> SceneCommand -> m [Scene m]
    applyCommand scenes' (SceneCommand.Push sceneId') = do
      newScene <- SceneMapping.fromId sceneId'
      pure $ newScene : scenes'

    applyCommand scenes' SceneCommand.Pop = pure $ drop 1 scenes'
    applyCommand _ SceneCommand.Clear = pure $ []

renderCurrentScene ::
  ( MonadReader r m
  , HasEnv r m
  , MonadIO m
  ) => m ()
renderCurrentScene = do
  scene <- getCurrentScene
  void $ traverse Scene.render scene

getCurrentScene :: (MonadReader r m, HasEnv r m, MonadIO m) => m (Maybe (Scene m))
getCurrentScene = do
  scenesRef' <- view (env.scenesRef)
  scenes' <- liftIO $ readIORef scenesRef'
  pure $ listToMaybe $ scenes'

delayCommand :: (MonadReader r m, HasEnv r m, MonadIO m) => SceneCommand -> m ()
delayCommand command' = do
  sceneCommandsRef' <- view (env.sceneCommandsRef)
  liftIO $ modifyIORef' sceneCommandsRef' (\commands -> commands ++ [command'])

delayPush :: (MonadReader r m, HasEnv r m, MonadIO m) => SceneId -> m ()
delayPush sceneId' = delayCommand (SceneCommand.Push sceneId')

delayPop :: (MonadReader r m, HasEnv r m, MonadIO m) => m ()
delayPop = delayCommand (SceneCommand.Pop)

delayClear :: (MonadReader r m, HasEnv r m, MonadIO m) => m ()
delayClear = delayCommand (SceneCommand.Clear)
