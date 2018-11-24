{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.LinearMotionSpec where

import Control.Lens
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Physics.Position
import CrossyToad.Physics.Direction
import CrossyToad.Physics.LinearMotion as LinearMotion

data Ent = Ent
  { __position :: Position
  , __linearMotion :: LinearMotion
  } deriving (Eq, Show)

makeClassy ''Ent

stationaryEnt :: Ent
stationaryEnt = Ent
  { __position = V2 0 0
  , __linearMotion = LinearMotion.mk East 0
  }

instance HasPosition Ent where
  position = _position

instance HasLinearMotion Ent where
  linearMotion = _linearMotion

spec_Physics_LinearMotion :: Spec
spec_Physics_LinearMotion = do
  describe "step" $ do
    let step' = stepBy 1

    it "should update the position by our speed" $ do
      let ent' = stationaryEnt & linearMotion %~ (speed .~ 2)
                                               . (direction .~ East)
      (step' ent') ^. position `shouldBe` (V2 2 0)

    it "should linearize speed" $ do
      let ent' = stationaryEnt & linearMotion %~ (speed .~ 20)
                                               . (direction .~ East)
      (stepBy 0.1 ent') ^. position `shouldBe` (V2 2 0)
