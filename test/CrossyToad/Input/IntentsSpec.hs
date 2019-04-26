module CrossyToad.Input.IntentsSpec where

import           Test.Tasty.Hspec

import qualified CrossyToad.Input.Intents as Intents
import qualified CrossyToad.Input.Intent as Intent
import qualified CrossyToad.Input.IntentEvent as IntentEvent

spec_Input_Intents :: Spec
spec_Input_Intents =
  describe "tick" $ do
    it "should tap tapped intents" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      Intents.tapped intents `shouldBe` [Intent.PauseOrExit]

    it "should cause previously tapped intents to become held" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [] intents
      Intents.held intents' `shouldBe` [Intent.PauseOrExit]

    it "should hold previously tapped intents if they are tapped again" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] intents
      Intents.held intents' `shouldBe` [Intent.PauseOrExit]

    it "should release released intents" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [IntentEvent.Release Intent.PauseOrExit] intents
      Intents.all intents' `shouldBe` []

    it "should not release other intents" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [IntentEvent.Release Intent.EnterOrConfirm] intents
      Intents.all intents' `shouldBe` [Intent.PauseOrExit]
