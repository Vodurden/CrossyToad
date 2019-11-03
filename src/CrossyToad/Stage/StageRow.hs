{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Stage.StageRow
  ( StageRow(..)
  , HasStageRow(..)
  , parser
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Text.Megaparsec.Extended
import           Text.Megaparsec.Char

import           CrossyToad.Physics.Speed (Speed)
import qualified CrossyToad.Physics.Speed as Speed
import           CrossyToad.Physics.Direction (Direction)
import qualified CrossyToad.Physics.Direction as Direction
import           CrossyToad.Stage.Entity (Entity)
import qualified CrossyToad.Stage.Entity as Entity
import           CrossyToad.Stage.GroundType (GroundType)
import qualified CrossyToad.Stage.GroundType as GroundType

data StageRow = StageRow
  { _entities :: [Entity]
  , _groundType :: GroundType
  , _entityDirection :: Direction
  , _entitySpeed :: Speed
  } deriving (Eq, Show)

makeClassy ''StageRow

parser :: Parser StageRow
parser = do
  void $ char '|'
  entities' <- many Entity.parser
  void $ char '|'
  groundType' <- GroundType.parser
  void $ space1
  direction' <- Direction.parser
  void $ space1
  speed' <- Speed.parser
  pure $ StageRow
    { _entities = entities'
    , _groundType = groundType'
    , _entityDirection = direction'
    , _entitySpeed = speed'
    }
