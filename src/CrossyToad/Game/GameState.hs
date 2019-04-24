{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module CrossyToad.Game.GameState where

import Control.Lens
import Linear.V2

import CrossyToad.Game.Vehicle (Car, Truck)
import CrossyToad.Game.RiverLog
import CrossyToad.Game.Toad as Toad
import CrossyToad.Game.ToadHome

data GameState = GameState
  { __toad :: !Toad
  , _toadHomes :: ![ToadHome]
  , _cars :: ![Car]
  , _trucks :: ![Truck]
  , _riverLogs :: ![RiverLog]
  } deriving (Eq, Show)

makeClassy ''GameState

instance HasToad GameState where toad = _toad

mk :: GameState
mk = GameState
  { __toad = Toad.mk (V2 0 0)
  , _toadHomes = []
  , _riverLogs = []
  , _cars = []
  , _trucks = []
  }
