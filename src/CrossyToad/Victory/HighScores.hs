{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Victory.HighScores
  ( HighScores
  , HasHighScores(..)
  , empty
  , toList
  , addScore
  , load
  , save
  , parser
  ) where

import           Control.Lens
import           Data.Bifunctor (first)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec.Char (space1)
import           Text.Megaparsec.Extended (Parser, sepEndBy, runParser, parseErrorPretty)

import           CrossyToad.Victory.HighScore (HighScore)
import qualified CrossyToad.Victory.HighScore as HighScore

data HighScores = HighScores
  { _scores :: Set HighScore
  }

makeClassy ''HighScores

empty :: HighScores
empty = HighScores Set.empty

toList :: HighScores -> [HighScore]
toList (HighScores scores') = Set.toDescList scores'

addScore :: HighScore -> HighScores -> HighScores
addScore score' = scores %~ (Set.insert score')

load :: String -> Text -> Either String HighScores
load fileName text = first parseErrorPretty $ runParser parser fileName text

save :: HighScores -> Text
save file = Text.intercalate "\n" $ HighScore.save <$> toList file

parser :: Parser HighScores
parser = do
  scores' <- sepEndBy HighScore.parser space1
  pure $ HighScores (Set.fromList scores')
