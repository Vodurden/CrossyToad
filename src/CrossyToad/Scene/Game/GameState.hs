{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.GameState where

import Control.Lens

import CrossyToad.Scene.Game.Toad

data GameState = GameState
  { __toad :: Toad
  } deriving (Eq, Show)

makeClassy ''GameState

instance HasToad GameState where
  toad = _toad

initialGameState :: GameState
initialGameState = GameState
  { __toad = initialToad
  }
