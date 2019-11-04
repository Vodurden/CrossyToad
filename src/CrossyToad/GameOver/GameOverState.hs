{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.GameOver.GameOverState
  ( GameOverState
  , HasGameOverState(..)
  , mk
  , letterUp
  , letterDown
  , nextLetter
  , previousLetter
  , currentName
  ) where

import Control.Lens
import Control.Zipper.Extended
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Bounded.Extended (succWrap, predWrap)
import Data.Bounded.ASCIIChar (ASCIIChar(..))

data GameOverState = GameOverState
  { _name :: Top :>> [ASCIIChar] :>> ASCIIChar
  , _totalScore :: !Int
  } deriving (Eq, Show)

makeClassy ''GameOverState

mk :: Int -> GameOverState
mk totalScore' = GameOverState
  { _name = zipper (ASCIIChar <$> "AAA") & fromWithin traverse
  , _totalScore = totalScore'
  }

currentName :: GameOverState -> Text
currentName state' = Text.pack $ unASCIIChar <$> (rezip $ state' ^. name)

letterUp :: GameOverState -> GameOverState
letterUp = name . focus %~ succWrap

letterDown :: GameOverState -> GameOverState
letterDown = name . focus %~ predWrap

nextLetter :: GameOverState -> GameOverState
nextLetter = name %~ wrapRight

previousLetter :: GameOverState -> GameOverState
previousLetter = name %~ wrapLeft
