{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module CrossyToad.Game.GameState where

import Control.Lens
import Linear.V2

import CrossyToad.Game.Terrain (Terrain)
import CrossyToad.Game.Toad as Toad
import CrossyToad.Game.ToadHome (ToadHome)
import CrossyToad.Game.Vehicle (Car, Truck, WoodLog)

data GameState = GameState
  { __toad :: !Toad
  , _deathTerrain :: ![Terrain]
  , _safeTerrain :: ![Terrain]
  , _toadHomes :: ![ToadHome]
  , _cars :: ![Car]
  , _trucks :: ![Truck]
  , _woodLogs :: ![WoodLog]
  } deriving (Eq, Show)

makeClassy ''GameState

instance HasToad GameState where toad = _toad

mk :: GameState
mk = GameState
  { __toad = Toad.mk (V2 0 0)
  , _deathTerrain = []
  , _safeTerrain = []
  , _toadHomes = []
  , _cars = []
  , _trucks = []
  , _woodLogs = []
  }
