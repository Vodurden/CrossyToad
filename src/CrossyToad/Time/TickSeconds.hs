{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.TickSeconds where

import Control.Lens

import CrossyToad.Time.Seconds

-- | The amount of seconds in this tick
newtype TickSeconds = TickSeconds { unTickSeconds :: Seconds }

makeClassy ''TickSeconds
