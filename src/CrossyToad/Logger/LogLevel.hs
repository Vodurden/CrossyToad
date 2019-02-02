{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Logger.LogLevel
  ( LogLevel(..)
  , HasLogLevel(..)
  , all
  , toPretty
  ) where

import Control.Lens
import Data.Text (Text)
import Prelude hiding (all)

data LogLevel
  = Error
  | Debug
  deriving (Eq, Show, Ord)

makeClassy ''LogLevel

all :: [LogLevel]
all = [Error, Debug]

toPretty :: LogLevel -> Text
toPretty Error = "[ERROR]"
toPretty Debug = "[DEBUG]"
