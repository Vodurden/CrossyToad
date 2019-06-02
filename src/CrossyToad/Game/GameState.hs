{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module CrossyToad.Game.GameState where

import Control.Lens
import Linear.V2

import CrossyToad.Game.Terrain (Terrain)
import CrossyToad.Game.Toad as Toad
import CrossyToad.Game.ToadHome (ToadHome)
import CrossyToad.Game.Vehicle (Car, Truck, WoodLog)
import CrossyToad.Game.Turtle (Turtle)
import CrossyToad.Game.Croc (Croc, CrocHead(..), CrocBody(..))

data GameState = GameState
  { __toad :: !Toad
  , _deathTerrain :: ![Terrain]
  , _safeTerrain :: ![Terrain]
  , _toadHomes :: ![ToadHome]
  , _cars :: ![Car]
  , _trucks :: ![Truck]
  , _turtles :: ![Turtle]
  , _crocs :: ![Croc]
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
  , _turtles = []
  , _crocs = []
  , _woodLogs = []
  }

crocHeads :: Lens' GameState [CrocHead]
crocHeads = lens getCrocHeads setCrocHeads
  where
    getCrocHeads :: GameState -> [CrocHead]
    getCrocHeads gameState' = CrocHead <$> (gameState' ^. crocs)

    setCrocHeads :: GameState -> [CrocHead] -> GameState
    setCrocHeads gameState' crocHeads' = gameState' & crocs .~ (unCrocHead <$> crocHeads')

crocBodies :: Lens' GameState [CrocBody]
crocBodies = lens getCrocBodys setCrocBodys
  where
    getCrocBodys :: GameState -> [CrocBody]
    getCrocBodys gameState' = CrocBody <$> (gameState' ^. crocs)

    setCrocBodys :: GameState -> [CrocBody] -> GameState
    setCrocBodys gameState' crocBodys' = gameState' & crocs .~ (unCrocBody <$> crocBodys')
