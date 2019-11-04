{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Stage.Entity
  ( Entity(..)
  , AsEntity(..)
  , parser
  , width
  ) where

import Control.Lens
import Text.Megaparsec.Extended
import Text.Megaparsec.Char

data Entity
  = NoEntity
  | Car
  | SportsCar
  | FarmTractor
  | Truck
  | Turtle
  | DivingTurtle
  | GrassSnake
  | Log Int
  | Croc
  | ToadHome
  deriving (Eq, Show)

makeClassyPrisms ''Entity

parser :: Parser Entity
parser = choice
  [ -- Unambiguous matches
    NoEntity <$ char ' '
  , SportsCar <$ char 'S'
  , FarmTractor <$ char 'F'
  , DivingTurtle <$ char 'D'
  , GrassSnake <$ char 'G'
  , ToadHome <$ char 'H'

  -- Ambiguous long matches
  , Croc <$ string "CRC"
  , Truck <$ string "TR"

  -- Ambigous short matches
  , Car <$ char 'C'
  , Turtle <$ char 'T'

  -- Logs
  , Log 6 <$ string "LLLLLL"
  , Log 5 <$ string "LLLLL"
  , Log 4 <$ string "LLLL"
  , Log 3 <$ string "LLL"
  , Log 2 <$ string "LL"
  , Log 1 <$ char 'L'
  ]

width :: Entity -> Int
width NoEntity = 1
width Car = 1
width SportsCar = 1
width FarmTractor = 1
width Truck = 2
width Turtle = 1
width DivingTurtle = 1
width GrassSnake = 1
width (Log width') = width'
width Croc = 3
width ToadHome = 1
