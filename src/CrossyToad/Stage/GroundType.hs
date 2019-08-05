{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Stage.GroundType
  ( GroundType(..)
  , AsGroundType(..)
  , parser
  ) where

import Control.Lens
import Text.Megaparsec.Extended
import Text.Megaparsec.Char

data GroundType = Grass | Road | River | Swamp
  deriving (Eq, Show)

makeClassyPrisms ''GroundType

parser :: Parser GroundType
parser = choice
  [ Grass <$ string "Grass"
  , Road <$ string "Road"
  , River <$ string "River"
  , Swamp <$ string "Swamp"
  ]
