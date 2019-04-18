module CrossyToad.Time.Seconds where

type Seconds = Double

-- | Executes the given function with a fixed timestep
-- |
-- | stepSize: The minimum number of seconds to execute f with
-- | f: The function to execute with a fixed timestep
-- | seconds: The total amount of time to execute this step with
-- |
-- | Returns the final value and any remaining seconds
fixedStep :: Seconds -> (Seconds -> a -> a) -> Seconds -> a -> (a, Seconds)
fixedStep minimumStepSize f seconds value
  | seconds >= minimumStepSize =
    fixedStep minimumStepSize f (seconds - minimumStepSize) (f minimumStepSize value)
  | otherwise = (value, seconds)

fixedStepM :: (Monad m) => Seconds -> (Seconds -> a -> m a) -> Seconds -> a -> m (a, Seconds)
fixedStepM minimumStepSize f seconds value
  | seconds >= minimumStepSize = do
      nextValue <- f minimumStepSize value
      fixedStepM minimumStepSize f (seconds - minimumStepSize) nextValue
  | otherwise = pure (value, seconds)
