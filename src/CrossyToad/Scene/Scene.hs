{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Scene
  ( Scene(..)
  , AsScene(..)
  , HasScene(..)
  , step
  ) where

import Control.Lens
import Control.Monad.State (MonadState)

import CrossyToad.Effect.Renderer
import CrossyToad.Input.Input
import CrossyToad.Scene.Internal
import CrossyToad.Scene.Title.Title (stepTitle)

step :: (MonadState s m, HasScene s, Input m, Renderer m) => m ()
step = do
    s <- use scene
    step' s
  where
    step' Title = stepTitle
    step' Play = pure ()
    step' Quit = pure ()
