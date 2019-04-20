{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.GameState where

import Control.Lens
import Linear.V2

import CrossyToad.Game.Car
import CrossyToad.Game.RiverLog
import CrossyToad.Game.SpawnPoint
import CrossyToad.Game.Toad as Toad
import CrossyToad.Game.ToadHome

data GameState = GameState
  { __toad :: !Toad
  , _toadHomes :: ![ToadHome]
  , _cars :: ![Car]
  , _riverLogs :: ![RiverLog]
  , __spawnPoints :: ![SpawnPoint]
  } deriving (Eq, Show)

makeClassy ''GameState

instance HasToad GameState where toad = _toad
instance HasSpawnPoints GameState where spawnPoints = _spawnPoints

mk :: GameState
mk = GameState
  { __toad = Toad.mk (V2 0 0)
  , _toadHomes = []
  , _riverLogs = []
  , _cars = []
  , __spawnPoints = []
  }
