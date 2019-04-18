{-# LANGUAGE RankNTypes #-}

module CrossyToad.Game.Entity
  ( Entity(..)
  ) where

data Entity
  = Car
  | RiverLog
  deriving (Eq, Show)
