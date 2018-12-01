{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Logger.IO.Env where

import           Control.Lens
import           Data.Set (Set)
import qualified Data.Set as Set

import           CrossyToad.Effect.Logger.LogLevel

data Env = Env
  { _logLevels :: Set LogLevel
  }

makeClassy ''Env

initialize :: [LogLevel] -> Env
initialize logLevels' = Env
  { _logLevels = Set.fromList logLevels'
  }
