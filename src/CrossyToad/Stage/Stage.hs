{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Stage.Stage
  ( Stage(..)
  , HasStage(..)
  , fromStageFile
  ) where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List (scanl')

import           CrossyToad.Geometry.Position
import qualified CrossyToad.Geometry.Position as Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Physics.Speed
import           CrossyToad.Stage.Entity (Entity)
import qualified CrossyToad.Stage.Entity as Entity
import           CrossyToad.Stage.GroundType
import           CrossyToad.Stage.StageFile
import           CrossyToad.Stage.StageRow (StageRow, entityDirection, entitySpeed)
import qualified CrossyToad.Stage.StageRow as StageRow

data Stage = Stage
  { _entities :: Map Position (Entity, Direction, Speed)
  , _tiles :: Map Position GroundType
  , _width :: Int
  , _height :: Int
  }

makeClassy ''Stage

instance Semigroup Stage where
  (<>) s1 s2 = Stage
    { _entities = (s1 ^. entities) <> (s2 ^. entities)
    , _tiles = (s1 ^. tiles) <> (s2 ^. tiles)
    , _width = max (s1 ^. width) (s2 ^. width)
    , _height = (s1 ^. height) + (s2 ^. height)
    }

instance Monoid Stage where
  mempty = Stage
    { _entities = Map.empty
    , _tiles = Map.empty
    , _width = 0
    , _height = 0
    }

fromStageFile :: StageFile -> Stage
fromStageFile stageFile' =
  let rowsWithYPosition = zip (stageFile' ^. rows) [0..]
  in mconcat $ fromRowWithYPosition <$> rowsWithYPosition

fromRowWithYPosition :: (StageRow, Int) -> Stage
fromRowWithYPosition (row', yPosition) =
  let
    rowDirection = row' ^. entityDirection
    rowSpeed = row' ^. entitySpeed
    rowEntities = row' ^. StageRow.entities
    entities' = loadEntities yPosition rowDirection rowSpeed rowEntities

    width' = sum $ imap (\_ (ent, _, _) -> Entity.width ent) entities'

    tiles' = loadTiles yPosition width' (row' ^. StageRow.groundType)
  in Stage { _entities = entities'
           , _tiles = tiles'
           , _width = width'
           , _height = 1
           }

loadEntities
  :: Int
  -> Direction
  -> Speed
  -> [Entity]
  -> Map Position (Entity, Direction, Speed)
loadEntities yPosition direction' speed' entities' =
  let
    xGridPositions = drop 1 $ scanl' (\pos ent -> (Entity.width ent) + pos) 0 entities'
    gridPositions = zip xGridPositions (repeat yPosition)
    positions = (uncurry Position.fromGrid) <$> gridPositions
    entitiesByPosition = Map.fromList $ zip positions entities'
  in
    fmap (\ent -> (ent, direction', speed')) entitiesByPosition

loadTiles :: Int -> Int -> GroundType -> Map Position GroundType
loadTiles yPosition rowWidth groundType' =
  let
    positions = (uncurry Position.fromGrid) <$> zip [0..rowWidth] (repeat yPosition)
    tileList = (\pos -> (pos, groundType')) <$> positions
  in
    Map.fromList tileList
