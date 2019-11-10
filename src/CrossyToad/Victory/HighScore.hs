{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Victory.HighScore
  ( HighScore(..)
  , HasHighScore(..)
  , parser
  , save
  ) where

import           Control.Lens
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec.Char (upperChar, space1)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Text.Megaparsec.Extended (Parser, count)

data HighScore = HighScore
  { _name :: Text
  , _score :: Int
  } deriving (Eq, Show)

makeClassy ''HighScore

save :: HighScore -> Text
save highScore' = (highScore' ^. name) <> " " <> (Text.pack $ show $ highScore' ^. score)

parser :: Parser HighScore
parser = do
  name' <- Text.pack <$> count 3 upperChar
  space1
  score' <- (decimal :: Parser Int)
  pure $ HighScore name' score'

instance Ord HighScore where
  compare = comparing _score <> comparing _name
