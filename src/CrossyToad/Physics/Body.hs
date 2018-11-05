{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Body where

import Control.Lens
import Control.Monad.State (runState)

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

stepBody :: Body -> Body
stepBody body' =
  let (motionVector, nextMotion) = runState stepJumpMotion (body' ^. jumpMotion)
  in body' & (position %~ (+ motionVector))
          . (jumpMotion .~ nextMotion)
