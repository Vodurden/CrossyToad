{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Scene
  ( Scene(..)
  , AsScene(..)
  , HasScene(..)
  , Env(..)
  , HasEnv(..)
  , initialize
  , run
  , handleInput
  , step
  , render
  ) where

import           Control.Lens.Extended
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef
import           Data.Foldable (traverse_)

import           CrossyToad.Logger.MonadLogger (MonadLogger(..))
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand)
import           CrossyToad.Effect.Renderer.Renderer (Renderer, runRenderCommand, clearScreen, drawScreen)
import           CrossyToad.Input.InputState
import           CrossyToad.Input.MonadInput
import           CrossyToad.Scene.Env
import           CrossyToad.Scene.Game.Game (HasGameState(..))
import qualified CrossyToad.Scene.Game.Game as Game
import           CrossyToad.Scene.Internal
import           CrossyToad.Scene.SceneState
import qualified CrossyToad.Scene.Title.Title as Title
import           CrossyToad.Time.MonadTime

-- | Runs all the computations for the active scene
run ::
  ( MonadReader r m
  , HasEnv r
  , MonadInput m
  , MonadLogger m
  , MonadTime m
  , Renderer m
  , MonadIO m
  ) => m Scene
run = do
    sceneStateRef' <- view (env.sceneStateRef)
    sceneState' <- liftIO $ readIORef sceneStateRef'

    -- Handle our input
    inputState' <- getInputState
    let sceneState'' = handleInput inputState' sceneState'

    -- Step the scene
    sceneState''' <- step sceneState''

    -- If we changed scenes, initialize the new scene.
    let sceneState'''' =
          if (sceneState' ^. scene /= sceneState''' ^. scene)
            then onTransition sceneState'''
            else sceneState'''

    -- Render the scene
    clearScreen
    let renderCommands = render sceneState''''
    traverse_ runRenderCommand renderCommands
    drawScreen

    -- Update our state
    liftIO $ writeIORef sceneStateRef' sceneState''''

    pure $ sceneState'''' ^. scene

handleInput :: (HasScene ent, HasGameState ent)
            => InputState
            -> ent
            -> ent
handleInput inputState' ent =
  case (ent^.scene) of
    Title -> Title.handleInput inputState' ent
    Game -> Game.handleInput inputState' ent
    Quit -> ent

step ::
  forall ent m.
  ( HasScene ent
  , HasGameState ent
  , MonadLogger m
  , MonadTime m
  ) => ent -> m ent
step ent = do
    step' (ent ^. scene) $ ent
  where
    step' :: Scene -> ent -> m ent
    step' Title = pure
    step' Game = Game.step
    step' Quit = pure

render :: (HasScene ent, HasGameState ent) => ent -> [RenderCommand]
render ent =
  case (ent ^. scene) of
    Title -> Title.render
    Game -> Game.render ent
    Quit -> []

onTransition :: forall ent. (HasScene ent, HasGameState ent) => ent -> ent
onTransition ent' = initialize' (ent' ^. scene) ent'
  where
    initialize' :: Scene -> ent -> ent
    initialize' Title = id
    initialize' Game = Game.initialize
    initialize' Quit = id
