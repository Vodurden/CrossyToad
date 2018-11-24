{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Scene
  ( Scene(..)
  , AsScene(..)
  , HasScene(..)
  , initialize
  , step
  ) where

import           Control.Lens.Extended
import           Control.Monad.State (StateT, State)

import           CrossyToad.Input.Input
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Scene.Game.Game (stepGame, HasGameState(..))
import qualified CrossyToad.Scene.Game.Game as Game
import           CrossyToad.Scene.Internal
import           CrossyToad.Scene.Title.Title (stepTitle)
import           CrossyToad.Time.Time

initialize :: (HasScene s, HasGameState s) => State s ()
initialize = do
    s <- use scene
    initialize' s
  where
    initialize' Title = pure ()
    initialize' Game = Game.initialize
    initialize' Quit = pure ()

step :: (HasScene s, HasGameState s, Input m, Renderer m, Time m) => StateT s m ()
step = do
    scene' <- use scene
    step' scene'
  where
    step' Title = stepTitle
    step' Game = stepGame
    step' Quit = pure ()
