module CrossyToad.Input.IntentsSpec where

import           Test.Tasty.Hspec

import qualified CrossyToad.Input.Intents as Intents
import qualified CrossyToad.Input.Intent as Intent
import qualified CrossyToad.Input.IntentEvent as IntentEvent

spec_Input_Intents :: Spec
spec_Input_Intents =
  describe "tick" $ do
    it "should intend to do tapped intents once" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      Intents.once intents `shouldBe` [Intent.PauseOrExit]

    it "should intend to do previously tapped intents continually" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [] intents
      Intents.continuous intents' `shouldBe` [Intent.PauseOrExit]

    it "should continue previously tapped intents if they are tapped again" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] intents
      Intents.continuous intents' `shouldBe` [Intent.PauseOrExit]

    it "should not intend to do released intents" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [IntentEvent.Release Intent.PauseOrExit] intents
      Intents.all intents' `shouldBe` []

    it "should not release other intents" $ do
      let intents = Intents.tick [IntentEvent.Tap Intent.PauseOrExit] Intents.initialize
      let intents' = Intents.tick [IntentEvent.Release Intent.EnterOrConfirm] intents
      Intents.all intents' `shouldBe` [Intent.PauseOrExit]
