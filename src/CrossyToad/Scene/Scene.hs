{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Scene
  ( Scene(..)
  , AsScene(..)
  , HasScene(..)
  , initialize
  , step
  ) where

import           Control.Lens.Extended

import           CrossyToad.Input.Input
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Scene.Game.Game (stepGame, HasGameState(..))
import qualified CrossyToad.Scene.Game.Game as Game
import           CrossyToad.Scene.Internal
import           CrossyToad.Scene.Title.Title (stepTitle)
import           CrossyToad.Time.Time

initialize :: forall ent. (HasScene ent, HasGameState ent) => ent -> ent
initialize ent' = initialize' (ent' ^. scene) ent'
  where
    initialize' :: Scene -> ent -> ent
    initialize' Title = id
    initialize' Game = Game.initialize
    initialize' Quit = id

step ::
  forall ent m.
  ( HasScene ent
  , HasGameState ent
  , Input m
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
