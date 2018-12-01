{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Scene
  ( Scene(..)
  , AsScene(..)
  , HasScene(..)
  , Env(..)
  , HasEnv(..)
  , initialize
  , stepIO
  , step
  ) where

import           Control.Lens.Extended
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef

import           CrossyToad.Effect.Input.Input
import           CrossyToad.Effect.Logger.Logger
import           CrossyToad.Effect.Renderer.Renderer
import           CrossyToad.Scene.Env
import           CrossyToad.Scene.Game.Game (stepGame, HasGameState(..))
import qualified CrossyToad.Scene.Game.Game as Game
import           CrossyToad.Scene.Internal
import           CrossyToad.Scene.SceneState
import           CrossyToad.Scene.Title.Title (stepTitle)
import           CrossyToad.Effect.Time.Time

-- | Steps the Scene
-- |
-- | Returns the current scene
-- |
-- | TODO:
-- |
-- | - Factor out Scene so we don't need MonadIO and the IORefs here
stepIO ::
  ( MonadReader r m
  , HasEnv r
  , Input m
  , Logger m
  , Renderer m
  , Time m
  , MonadIO m
  ) => m (Scene)
stepIO = do
  -- Step the active scene
  sceneStateRef' <- view (env.sceneStateRef)
  screenState' <- liftIO $ readIORef sceneStateRef'
  nextScreenState <- step screenState'

  -- If we changed scenes, initialize the new scene.
  let newScreenState =
        if (screenState' ^. scene /= nextScreenState ^. scene)
          then onTransition nextScreenState
          else nextScreenState

  liftIO $ writeIORef sceneStateRef' newScreenState

  pure (newScreenState ^. scene)

step ::
  forall ent m.
  ( HasScene ent
  , HasGameState ent
  , Input m
  , Logger m
  , Renderer m
  , Time m
  ) => ent -> m ent
step ent = do
    step' (ent ^. scene) $ ent
  where
    step' :: Scene -> ent -> m ent
    step' Title = stepTitle
    step' Game = stepGame
    step' Quit = pure

onTransition :: forall ent. (HasScene ent, HasGameState ent) => ent -> ent
onTransition ent' = initialize' (ent' ^. scene) ent'
  where
    initialize' :: Scene -> ent -> ent
    initialize' Title = id
    initialize' Game = Game.initialize
    initialize' Quit = id
