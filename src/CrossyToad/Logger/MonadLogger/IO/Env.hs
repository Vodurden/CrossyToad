{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Logger.MonadLogger.IO.Env where

import           Control.Lens
import           Data.Set (Set)
import qualified Data.Set as Set

import           CrossyToad.Logger.LogLevel

data Env = Env
  { _logLevels :: Set LogLevel
  }

makeClassy ''Env

initialize :: [LogLevel] -> Env
initialize logLevels' = Env
  { _logLevels = Set.fromList logLevels'
  }
