module CrossyToad.Title.MenuOption
  ( MenuOption(..)
  , allOptions
  , text
  ) where

import Data.Text (Text)

data MenuOption
  = StartGame
  | HighScores
  | Credits
  | Quit
  deriving (Eq, Show, Ord, Enum, Bounded)

allOptions :: [MenuOption]
allOptions = [StartGame, HighScores, Credits, Quit]

text :: MenuOption -> Text
text StartGame = " Start Game "
text HighScores = "High Scores"
text Credits = "  Credits  "
text Quit = "    Quit    "
