{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Victory.Scorable
  ( Scorable(..)
  , HasScorable(..)
  , mk
  ) where

import           Control.Lens

-- | Represents an entity that is worth score when collided with
data Scorable = Scorable
  { _value :: !Int
  , _collected :: !Bool
  } deriving (Eq, Show)

makeClassy ''Scorable

mk :: Int -> Scorable
mk value' = Scorable value' False
