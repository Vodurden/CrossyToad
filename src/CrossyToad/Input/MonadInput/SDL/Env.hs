{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.MonadInput.SDL.Env where

import Control.Lens
import Data.IORef

import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Input.Intents as Intents

data Env = Env
  { _intentsRef :: IORef Intents
  }

makeClassy ''Env

initialize :: IO Env
initialize = Env <$> (newIORef Intents.initialize)
