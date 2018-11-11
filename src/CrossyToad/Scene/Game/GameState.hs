{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.GameState where

import Control.Lens

import CrossyToad.Scene.Game.Toad
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
  { __toad = initialToad
  , _cars = [initialCar]
  }
