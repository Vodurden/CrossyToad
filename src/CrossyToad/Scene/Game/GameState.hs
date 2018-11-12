{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.GameState where

import Control.Lens
import Linear.V2

import CrossyToad.Scene.Game.Toad as Toad
import CrossyToad.Scene.Game.Car

data GameState = GameState
  { __toad :: Toad
  , _cars :: [Car]
  } deriving (Eq, Show)

makeClassy ''GameState

instance HasToad GameState where
  toad = _toad

initialGameState :: GameState
initialGameState = GameState
  { __toad = Toad.mk (V2 0 0)
  , _cars = []
  }
