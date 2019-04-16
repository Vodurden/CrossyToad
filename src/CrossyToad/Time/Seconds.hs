module CrossyToad.Time.Seconds where

import Control.Arrow ((>>>))

type Seconds = Float

-- | Executes the given function with a fixed timesteo
-- |
-- | stepSize: The minimum number of seconds to execute f with
-- | f: The function to execute with a fixed timestep
-- | seconds: The total amount of time to execute this step with
fixedStep :: Seconds -> (Seconds -> a -> a) -> Seconds -> a -> a
fixedStep minimumStepSize f seconds
  | seconds >= minimumStepSize = (f minimumStepSize) >>> (fixedStep minimumStepSize f (minimumStepSize - seconds))
  | otherwise = id
