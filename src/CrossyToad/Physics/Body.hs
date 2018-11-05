{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Body where

import Control.Lens
import Control.Monad.State (runState)

import CrossyToad.Time.Time
import CrossyToad.Physics.Position
import CrossyToad.Physics.JumpMotion

-- | Represents the physical body of an entity within the game
data Body = Body
  { __position :: Position
  , __jumpMotion :: JumpMotion
  } deriving (Eq, Show)

makeClassy ''Body

instance HasPosition Body where
  position = _position

instance HasJumpMotion Body where
  jumpMotion = _jumpMotion

stepBody :: (Time m) => Body -> m Body
stepBody body' = do
  delta <- deltaTime
  let (motionVector', nextMotion') = runState (stepJumpMotion delta) (body' ^. jumpMotion)
  pure $ body' & (position %~ (+ motionVector'))
               . (jumpMotion .~ nextMotion')
