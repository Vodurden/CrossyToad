{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.MonadTime.SDL.Env where

import Control.Lens
import Data.IORef

import CrossyToad.Time.MonadTime.SDL.MonadTimeState

data Env = Env
  { __monadTimeStateRef :: IORef MonadTimeState
  }

makeClassy ''Env

instance HasMonadTimeStateIORef Env where
  monadTimeStateRef = _monadTimeStateRef

initialize :: IO Env
initialize = do
  initialMonadTimeState' <- initialMonadTimeState
  Env <$> (newIORef initialMonadTimeState')
