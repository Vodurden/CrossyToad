module CrossyToad.Physics.JumpMotionSpec where

import Control.Lens
import Control.Monad.State (evalState, execState)
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Physics.Direction
import CrossyToad.Physics.JumpMotion

stationaryMotion :: JumpMotion
stationaryMotion = JumpMotion
  { __direction = East
  , _speed = 0
  , _distance = 32
  , _targetDistance = 0
  }

movingMotion :: JumpMotion
movingMotion = stationaryMotion
  { _speed = 1
  , _targetDistance = 32
  }

spec_Physics_JumpMotion :: Spec
spec_Physics_JumpMotion = do
  describe "jump" $ do
    it "should not update anything if we are already moving" $ do
      let motion' = movingMotion & (direction .~ East)
      (jump West motion') `shouldBe` motion'

    it "should update the target distance if we are not moving" $ do
      let distance' = (jump (movingMotion^.direction) stationaryMotion)^.targetDistance
      distance' `shouldBe` stationaryMotion^.distance

    it "should update the direction if we are not moving" $ do
      let motion' = stationaryMotion & (direction .~ East)
      let direction' = (jump West motion') ^. direction
      direction' `shouldBe` West

  describe "isMoving" $ do
    it "should be true when we are moving" $ isMoving movingMotion `shouldBe` True
    it "should be false when we are stationary" $ isMoving stationaryMotion `shouldBe` False

  describe "stepJumpMotion" $ do
    context "when the frame rate is constant" $ do
      let stepJumpMotion' = stepJumpMotion 1
      context "should return a motion vector that" $ do
        it "stops at the target distance when the speed exceeds the distance" $ do
          let motion' = movingMotion & (speed .~ 2)
                                     . (targetDistance .~ 1)
                                     . (direction .~ East)
          evalState stepJumpMotion' motion' `shouldBe` (V2 1 0)

        it "moves by the speed of the motion" $ do
          let motion' = movingMotion & (speed .~ 2)
                                     . (targetDistance .~ 5)
                                     . (direction .~ East)
          evalState stepJumpMotion' motion' `shouldBe` (V2 2 0)

        it "does nothing when the speed is 0" $ do
          let motion' = movingMotion & speed .~ 0
          evalState stepJumpMotion' motion' `shouldBe` (V2 0 0)

        it "does nothing when the target distance is 0" $ do
          let motion' = movingMotion & targetDistance .~ 0
          evalState stepJumpMotion' motion' `shouldBe` (V2 0 0)

      it "should reduce the target distance by the travelled distance" $ do
        let motion' = movingMotion & (speed .~ 5)
                                   . (targetDistance .~ 11)
        let nextMotion = execState stepJumpMotion' motion'
        nextMotion ^. targetDistance `shouldBe` 6

      it "should not reduce the target distance below 0" $ do
        let motion' = movingMotion & (speed .~ 5)
                                   . (targetDistance .~ 4.9)
        let nextMotion = execState stepJumpMotion' motion'
        nextMotion ^. targetDistance `shouldBe` 0

    context "when the frame rate varies" $ do
      it "should linearize the result against the delta time" $ do
        let motion' = movingMotion & (speed .~ 10)
                                   . (targetDistance .~ 10)
                                   . (direction .~ East)
        let delta = 0.1
        evalState (stepJumpMotion delta) motion' `shouldBe` (V2 1 0)

      it "should only linearize speed" $ do
        let motion' = movingMotion & (speed .~ 320)
                                   . (targetDistance .~ 32)
                                   . (direction .~ East)
        let delta = 0.05
        evalState (stepJumpMotion delta) motion' `shouldBe` (V2 16 0)

      it "should update the target distance by the linearized amount" $ do
        let motion' = movingMotion & (speed .~ 10)
                                   . (targetDistance .~ 10)
                                   . (direction .~ East)
        let delta = 0.1
        let nextMotion = execState (stepJumpMotion delta) motion'
        nextMotion ^.targetDistance `shouldBe` 9
