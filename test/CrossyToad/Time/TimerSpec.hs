module CrossyToad.Time.TimerSpec where

import Control.Lens
import Control.Monad.State.Strict (execState, evalState)

import Test.Tasty.Hspec

import CrossyToad.Time.Timer as Timer

spec_Time_Timer :: Spec
spec_Time_Timer = do
  describe "start" $ do
    it "should set the current time to the start time" $ do
      let timer' = Timer.start $ Timer.mk 10
      timer' ^. currentTime `shouldBe` 10

  describe "running" $ do
    it "should return true if the timer has any current time remaining" $ do
      let timer' = Timer.start $ Timer.mk 1
      Timer.running timer' `shouldBe` True

    it "should return false if the timer has not been started" $ do
      Timer.running (Timer.mk 1) `shouldBe` False

    it "should return false if the timer has run out of time" $ do
      Timer.running (Timer.mk 1 & currentTime .~ 0) `shouldBe` False

  describe "tickBy" $ do
    it "should reduce the current time by the given delta time" $ do
      let timer' = Timer.start $ Timer.mk 10
      (execState (Timer.tickBy 2) timer') ^. currentTime `shouldBe` 8

    it "should not reduce the current time below 0" $ do
      let timer' = Timer.start $ Timer.mk 1
      (execState (Timer.tickBy 2) timer') ^. currentTime `shouldBe` 0

    it "should return any delta time not used by the cooldown" $ do
      let timer' = Timer.start $ Timer.mk 3
      let remainingDelta = evalState (Timer.tickBy 5) timer'
      remainingDelta `shouldBe` 2

    it "should not return any extra delta time if the timer has not finished" $ do
      let timer' = Timer.start $ Timer.mk 3
      let remainingDelta = evalState (Timer.tickBy 1) timer'
      remainingDelta `shouldBe` 0
