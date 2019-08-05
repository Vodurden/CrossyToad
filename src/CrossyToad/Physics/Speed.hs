module CrossyToad.Physics.Speed
  ( Speed
  , HasSpeed(..)
  , secondsPerTile
  , parser
  ) where

import Control.Lens
import Text.Megaparsec.Extended
import Text.Megaparsec.Char.Lexer (decimal, float)

import CrossyToad.Time.Seconds

-- | How fast an object moves per second in pixels
type Speed = Double

class HasSpeed a where
  speed :: Lens' a Speed

instance HasSpeed Speed where speed = id

secondsPerTile :: Seconds -> Speed
secondsPerTile seconds = 64 * (1 / seconds)

-- | Parses integers into speed.
-- |
-- | We assume all speed is defined as "seconds-per-tile",
-- | i.e. 1.2 is "1.2 seconds per tile"
parser :: Parser Speed
parser =
  secondsPerTile <$> try float <|> (fromIntegral <$> (decimal :: Parser Integer))
