{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Logger.LogLevel where

import Control.Lens
import Data.Text (Text)

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
