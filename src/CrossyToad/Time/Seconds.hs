module CrossyToad.Time.Seconds where

type Seconds = Double

-- | Executes the given function with a fixed timetick
-- |
-- | tickSize: The minimum number of seconds to execute f with
-- | f: The function to execute with a fixed timetick
-- | seconds: The total amount of time to execute this tick with
-- |
-- | Returns the final value and any remaining seconds
fixedTick :: Seconds -> (Seconds -> a -> a) -> Seconds -> a -> (a, Seconds)
fixedTick minimumTickSize f seconds value
  | seconds >= minimumTickSize =
    fixedTick minimumTickSize f (seconds - minimumTickSize) (f minimumTickSize value)
  | otherwise = (value, seconds)

fixedTickM :: (Monad m) => Seconds -> (Seconds -> a -> m a) -> Seconds -> a -> m (a, Seconds)
fixedTickM minimumTickSize f seconds value
  | seconds >= minimumTickSize = do
      nextValue <- f minimumTickSize value
      fixedTickM minimumTickSize f (seconds - minimumTickSize) nextValue
  | otherwise = pure (value, seconds)
