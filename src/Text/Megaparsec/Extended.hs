module Text.Megaparsec.Extended
  ( module Text.Megaparsec
  , Parser
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text
