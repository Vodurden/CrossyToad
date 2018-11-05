module CrossyToad.Physics.JumpMotionSpec where

import Control.Lens
import Control.Monad.State (evalState, execState)
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Physics.Direction
import CrossyToad.Physics.JumpMotion

mkJumpMotion :: JumpMotion
mkJumpMotion = JumpMotion
  { __direction = East
  , _velocity = 0
  , _targetDistance = 0
  }

movingJumpMotion :: JumpMotion
movingJumpMotion = mkJumpMotion
  { _velocity = 1
  , _targetDistance = 32
  }

spec_Physics_JumpMotion :: Spec
spec_Physics_JumpMotion =
  describe "stepJumpMotion" $ do
    context "when the frame rate is constant" $ do
      let stepJumpMotion' = stepJumpMotion 1
      context "should return a motion vector that" $ do
        it "stops at the target distance when the velocity exceeds the distance" $ do
          let motion' = movingJumpMotion & (velocity .~ 2)
                                         . (targetDistance .~ 1)
                                         . (direction .~ East)
          evalState stepJumpMotion' motion' `shouldBe` (V2 1 0)

        it "has the correct velocity" $ do
          let motion' = movingJumpMotion & (velocity .~ 2)
                                         . (targetDistance .~ 5)
                                         . (direction .~ East)
          evalState stepJumpMotion' motion' `shouldBe` (V2 2 0)

        it "does nothing when the velocity is 0" $ do
          let motion' = movingJumpMotion & velocity .~ 0
          evalState stepJumpMotion' motion' `shouldBe` (V2 0 0)

        it "does nothing when the target distance is 0" $ do
          let motion' = movingJumpMotion & targetDistance .~ 0
          evalState stepJumpMotion' motion' `shouldBe` (V2 0 0)

      it "should reduce the target distance by the travelled distance" $ do
        let motion' = movingJumpMotion & (velocity .~ 5)
                                       . (targetDistance .~ 11)
        let nextMotion = execState stepJumpMotion' motion'
        nextMotion ^. targetDistance `shouldBe` 6

      it "should not reduce the target distance below 0" $ do
        let motion' = movingJumpMotion & (velocity .~ 5)
                                       . (targetDistance .~ 4.9)
        let nextMotion = execState stepJumpMotion' motion'
        nextMotion ^. targetDistance `shouldBe` 0

    context "when the frame rate varies" $ do
      it "should linearize the result against the delta time" $ do
        let motion' = movingJumpMotion & (velocity .~ 10)
                                       . (targetDistance .~ 10)
                                       . (direction .~ East)
        let delta = 0.1
        evalState (stepJumpMotion delta) motion' `shouldBe` (V2 1 0)

      it "should only linearize velocity" $ do
        let motion' = movingJumpMotion & (velocity .~ 320)
                                       . (targetDistance .~ 32)
                                       . (direction .~ East)
        let delta = 0.05
        evalState (stepJumpMotion delta) motion' `shouldBe` (V2 16 0)

      it "should update the target distance by the linearized amount" $ do
        let motion' = movingJumpMotion & (velocity .~ 10)
                                      . (targetDistance .~ 10)
                                      . (direction .~ East)
        let delta = 0.1
        let nextMotion = execState (stepJumpMotion delta) motion'
        nextMotion ^.targetDistance `shouldBe` 9
