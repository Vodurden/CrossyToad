module CrossyToad.Physics.JumpMotionSpec where

import Control.Lens
import Control.Monad.State (evalState, execState)
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Physics.Direction
import CrossyToad.Physics.JumpMotion

movingMotion :: JumpMotion
movingMotion = initialJumpMotion
  { _speed = 1
  , _targetDistance = 32
  }

spec_Physics_JumpMotion :: Spec
spec_Physics_JumpMotion = do
  describe "jump" $ do
    it "should not jump if we are already moving" $ do
      let motion' = movingMotion & (direction .~ East)
      (jump West motion') `shouldBe` motion'

    it "should not jump if we are cooling down" $ do
      let motion' = initialJumpMotion & (currentCooldown .~ 0.5)
      (jump East motion') `shouldBe` motion'

    it "should update the target distance if we are not moving" $ do
      let distance' = (jump (movingMotion^.direction) initialJumpMotion)^.targetDistance
      distance' `shouldBe` initialJumpMotion^.distance

    it "should update the direction if we are not moving" $ do
      let motion' = initialJumpMotion & (direction .~ East)
      let direction' = (jump West motion') ^. direction
      direction' `shouldBe` West

  describe "isMoving" $ do
    it "should be true when we are moving" $ isMoving movingMotion `shouldBe` True
    it "should be false when we are stationary" $ isMoving initialJumpMotion `shouldBe` False

  describe "stepJumpMotion" $ do
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

    it "should reduce the current cooldown by the delta" $ do
      let motion' = initialJumpMotion & (currentCooldown .~ 2)
      let nextMotion = execState (stepJumpMotion 1) motion'
      nextMotion ^. currentCooldown `shouldBe` 1

    it "should linearize the result against the remaining delta time after cooldown" $ do
      let motion' = movingMotion & (speed .~ 10)
                                 . (targetDistance .~ 10)
                                 . (direction .~ East)
                                 . (currentCooldown .~ 0.1)
      let delta = 0.2
      evalState (stepJumpMotion delta) motion' `shouldBe` (V2 1 0)

    it "should begin cooling down when finishing a jump" $ do
      let motion' = movingMotion & (speed .~ 10)
                                 . (targetDistance .~ 5)
                                 . (cooldown .~ 0.2)
      let nextMotion = execState (stepJumpMotion 1) motion'
      nextMotion ^. currentCooldown `shouldBe` 0.2

    it "should not cooldown if a jump has not finished yet" $ do
      let motion' = movingMotion & (speed .~ 10)
                                 . (targetDistance .~ 11)
                                 . (cooldown .~ 0.2)
      let nextMotion = execState (stepJumpMotion 1) motion'
      nextMotion ^. currentCooldown `shouldBe` 0
