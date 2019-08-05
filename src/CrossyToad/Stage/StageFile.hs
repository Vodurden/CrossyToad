{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Stage.StageFile
  ( StageFile(..)
  , HasStageFile(..)
  , load
  , parser
  ) where

import           Control.Lens
import           Data.Bifunctor (first)
import           Data.Text (Text)
import           Text.Megaparsec.Extended
import           Text.Megaparsec.Char (space1)

import           CrossyToad.Stage.StageRow (StageRow)
import qualified CrossyToad.Stage.StageRow as StageRow

-- | A stage in CrossyToad consists of a set of rows which define:
-- |
-- | - The positions of all entities in the row
-- | - The direction of all entities in the row
-- | - The speed of all entities in the row
-- | - The ground the row is made of
-- |
-- | Stages are encoded using an ASCII like so:
-- |
-- |    |   H    H    H    H   |Swamp East 0
-- |    | TTT TTT DDD TTT TTT  |River East 0.75
-- |    |   LLLL LLLL LLLL LLLL|River East 0.75
-- |    | LL  LL  LL  LL  LL   |River West 1
-- |    |TT TT TT TT DD TT TT  |River West 1
-- |    |                      |Grass East 0
-- |    |             TR    TR |Road  East 0.5
-- |    | TR      TR           |Road  East 0.5
-- |    |F         F           |Road  East 0.5
-- |    |C   C   C   C   C     |Road  East 0.5
-- |    |  C     C     C    C  |Road  East 0.5
-- |    |                      |Grass East 0
-- |
-- | The area within the pipes denotes the physical area of the game. With each
-- | character corresponding to an entity within the game. Typically each character
-- | also indicates an entitity who's size is a single tile.
-- |
-- | The following entities are available:
-- |
-- | - S: Sports car
-- | - C: Car
-- | - F: Farm Tractor
-- | - TR: Truck, Trucks must always be two characters in length
-- | - T: Turtle
-- | - D: Diving Turtle
-- | - L: Log
-- | - CRC: Croc, Crocodiles must always be three characters in length
-- | - H: Toad Home
-- |
-- | After the pipes we have the row metadata. This includes what tiles the row is
-- | made out of, the direction of the entities in the row and the speed of the
-- | entities in the row.
-- |
data StageFile = StageFile
  { _rows :: [StageRow]
  } deriving (Eq, Show)

makeClassy ''StageFile

load :: String -> Text -> Either String StageFile
load fileName text = first parseErrorPretty $ runParser parser fileName text

parser :: Parser StageFile
parser = do
  rows' <- sepEndBy StageRow.parser space1
  pure $ StageFile rows'
