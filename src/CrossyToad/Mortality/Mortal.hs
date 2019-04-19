{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Mortality.Mortal
  ( Mortal(..)
  , HasMortal(..)
  , mk
  , die
  ) where

import Control.Lens

import CrossyToad.Geometry.Position (Position, HasPosition(..))

-- | Things that are mortal can be killed and will
-- | respawn at the given position if they still have lives
data Mortal = Mortal
  { _lives :: !Int
  , _respawnPosition :: !Position
  } deriving (Eq, Show)

makeClassy ''Mortal

mk :: Int -> Position -> Mortal
mk = Mortal

-- | Kills the entity
die :: (HasPosition ent, HasMortal ent) => ent -> ent
die ent = ent & (lives .~ max 0 (ent ^. lives - 1))
              . (position .~ ent ^. respawnPosition)
