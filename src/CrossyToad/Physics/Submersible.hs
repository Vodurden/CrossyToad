{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Submersible
  ( Submersible
  , HasSubmersible(..)
  , mk
  , tick
  , progress
  , swimming
  , sunk
  ) where

import Control.Lens

import CrossyToad.Time.Seconds (Seconds)

data Submersible = Submersible
  { _swimTime :: Seconds -- ^ The amount of time this entity should swim before sinking
  , _sinkTime :: Seconds -- ^ The amount of time this entity should stay submerged before swimming

  , _stateTime :: Seconds -- ^ The amount of time this entity has been in it's current state
  , _swimState :: SwimState
  } deriving (Eq, Show)

data SwimState = Swimming | Sinking
  deriving (Eq, Show)

makeClassy ''Submersible
makeClassyPrisms ''SwimState

mk :: Seconds -> Seconds -> Submersible
mk swimTime' sinkTime' = Submersible
  { _swimTime = swimTime'
  , _sinkTime = sinkTime'
  , _stateTime = 0
  , _swimState = Swimming
  }

tick :: (HasSubmersible ent) => Seconds -> ent -> ent
tick seconds ent' =
  ent' & (submersible.stateTime +~ seconds)
       & (submersible %~ sinkOrSwim)

-- | Returns how much time is remaining until the submersible
-- | transitions to it's next state as a Percentage. with 1.0
-- | indicating that the submersible will transition immediately
-- | and 0.0 indicating the submersible has the full duration to
-- | to go
progress :: (HasSubmersible ent) => ent -> Double
progress ent' = (ent' ^. stateTime) / (targetTime $ ent' ^. submersible)

remainingTime :: (HasSubmersible ent) => ent -> Seconds
remainingTime ent' = (targetTime $ ent' ^. submersible) - (ent' ^. submersible . stateTime)

swimming :: HasSubmersible ent => ent -> Bool
swimming ent = ent ^. swimState == Swimming

sunk :: HasSubmersible ent => ent -> Bool
sunk ent = ent ^. swimState == Sinking

-- | Transition from swimming to sinking or vice-versa if
-- | our state time is exceeded.
sinkOrSwim :: Submersible -> Submersible
sinkOrSwim ent'
  | remainingTime ent' <= 0 = ent' & swapState . (stateTime .~ abs (remainingTime ent'))
  | otherwise = ent'

swapState :: Submersible -> Submersible
swapState ent' =
  case ent' ^. swimState of
    Swimming -> ent' & swimState .~ Sinking
    Sinking -> ent' & swimState .~ Swimming

-- | Returns the time we need to reach to transition
targetTime :: Submersible -> Seconds
targetTime ent' =
  case ent' ^. swimState of
    Swimming -> ent' ^. swimTime
    Sinking -> ent' ^. sinkTime
