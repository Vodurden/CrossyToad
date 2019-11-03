{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.GameState
  ( GameState
  , HasGameState(..)
  , empty
  , fromStage
  , crocHeads
  , crocBodies
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Data.Foldable (foldl')
import qualified Data.Map as Map
import           Linear.V2

import           CrossyToad.Game.Croc (Croc, CrocHead(..), CrocBody(..))
import qualified CrossyToad.Game.Croc as Croc
import           CrossyToad.Game.Terrain (Terrain)
import qualified CrossyToad.Game.Terrain as Terrain
import           CrossyToad.Game.Toad as Toad
import           CrossyToad.Game.ToadHome (ToadHome)
import qualified CrossyToad.Game.ToadHome as ToadHome
import           CrossyToad.Game.Turtle (Turtle)
import qualified CrossyToad.Game.Turtle as Turtle
import           CrossyToad.Game.Vehicle (Car, SportsCar, Truck, WoodLog)
import qualified CrossyToad.Game.Vehicle as Vehicle
import           CrossyToad.Geometry.Position (Position)
import qualified CrossyToad.Geometry.Position as Position
import           CrossyToad.Physics.Direction (Direction)
import           CrossyToad.Physics.Speed (Speed)
import           CrossyToad.Stage.Entity (Entity)
import qualified CrossyToad.Stage.Entity as Entity
import           CrossyToad.Stage.GroundType (GroundType)
import qualified CrossyToad.Stage.GroundType as GroundType
import           CrossyToad.Stage.Stage (Stage, HasStage(..))

data GameState = GameState
  { __toad :: !Toad
  , _deathTerrain :: ![Terrain]
  , _safeTerrain :: ![Terrain]
  , _toadHomes :: ![ToadHome]
  , _cars :: ![Car]
  , _sportsCars :: ![SportsCar]
  , _trucks :: ![Truck]
  , _turtles :: ![Turtle]
  , _crocs :: ![Croc]
  , _woodLogs :: ![WoodLog]
  } deriving (Eq, Show)

makeClassy ''GameState

instance HasToad GameState where toad = _toad

empty :: GameState
empty = GameState
  { __toad = Toad.mk (V2 0 0)
  , _deathTerrain = []
  , _safeTerrain = []
  , _toadHomes = []
  , _cars = []
  , _sportsCars = []
  , _trucks = []
  , _turtles = []
  , _crocs = []
  , _woodLogs = []
  }

fromStage :: Stage -> GameState
fromStage stage' =
   (loadToad stage') >>> (loadEntities stage') >>> (loadTiles stage') $ empty
  where
    loadToad :: Stage -> GameState -> GameState
    loadToad stage'' gameState' =
      let toadX = truncate $ (fromIntegral $ stage'' ^. width) / (2 :: Float)
          toadY = stage'' ^. height
      in gameState' & toad .~ Toad.mk (Position.fromGrid toadX toadY)

    loadEntities :: Stage -> GameState -> GameState
    loadEntities stage'' gameState' =
      foldl' (flip loadEntity) gameState' (Map.toList $ stage'' ^. entities)

    loadEntity :: (Position, (Entity, Direction, Speed)) -> GameState -> GameState
    loadEntity (pos, (entity', dir, speed')) =
      case entity' of
        Entity.NoEntity -> id
        Entity.Car -> over cars (Vehicle.mkCar pos dir speed' :)
        Entity.SportsCar -> over sportsCars (Vehicle.mkSportsCar pos dir speed' :)
        Entity.FarmTractor -> over cars (Vehicle.mkCar pos dir speed' :)
        Entity.Truck -> over trucks (Vehicle.mkTruck pos dir speed' :)
        Entity.Turtle -> over turtles (Turtle.mk pos dir speed' :)
        Entity.DivingTurtle -> over turtles (Turtle.mk pos dir speed' :)
        Entity.Log -> over woodLogs (Vehicle.mkWoodLog pos dir speed' :)
        Entity.Croc -> over crocs (Croc.mk pos dir speed' :)
        Entity.ToadHome -> over toadHomes (ToadHome.mk pos :)

    loadTiles :: Stage -> GameState -> GameState
    loadTiles stage'' gameState' =
      foldl' (flip loadTile) gameState' (Map.toList $ stage'' ^. tiles)

    loadTile :: (Position, GroundType) -> GameState -> GameState
    loadTile (pos, groundType') =
      case groundType' of
        GroundType.Grass -> over safeTerrain (Terrain.mkGrass pos :)
        GroundType.Road -> over safeTerrain (Terrain.mkRoad pos :)
        GroundType.Swamp -> over deathTerrain (Terrain.mkSwamp pos :)
        GroundType.River -> over deathTerrain (Terrain.mkWater pos :)

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
