{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Time.Tick where

import Control.Lens
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State.Strict.Extended (StateT, lift, get)

import CrossyToad.Time.Seconds

type Tick m a = Coroutine (Request (Seconds -> a -> m a) Seconds) m ()

-- | Wait for some time
wait :: (Monad m) => Seconds -> Tick m a
wait seconds = after seconds (pure . id)

-- | Apply f after some time
after :: (Monad m) => Seconds -> (a -> m a) -> Tick m a
after seconds f | seconds <= 0 = do
                    _ <- request (const f)
                    pure ()
                | otherwise = do
                    dt <- request (const $ pure . id)
                    wait (seconds - dt)

-- | Apply f every interval
interval :: forall m a. (Monad m) => Seconds -> (Seconds -> a -> m a) -> Tick m a
interval = interval' 0
  where
    interval' :: Seconds -> Seconds -> (Seconds -> a -> m a) -> Tick m a
    interval' currentSeconds target tick'
      | currentSeconds >= target = do
        dt <- request tick'
        interval' (dt + currentSeconds - target) target tick'
      | otherwise = do
        dt <- request (const $ pure . id)
        interval' (dt + currentSeconds) target tick'

step :: (Monad m) => Seconds -> a -> StateT (Tick m a) m a
step delta input = do
  tick' <- get
  z <- lift $ resume tick'
  case z of
    (Left (Request tickF stateF)) -> do
      id .= stateF delta
      lift $ tickF input
    (Right _) -> do
      pure input

-- runTick :: Tick m a -> a -> m a
-- runTick tick' input = do
--   z <- tick' ^. tickf
--   pure $ case z of
--     (Left (Request seconds stateF)) -> input
--     (Right _) -> input

-- mkInterval :: Seconds -> (a -> m a) -> Tick m a
-- mkInterval interval f = Tick
