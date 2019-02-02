module CrossyToad.Sprite.AnimationSpec where

import           Control.Lens
import           Linear.V2

import           Test.Tasty.Hspec

import           CrossyToad.Renderer.Clip (HasClip(..))
import           CrossyToad.Time.Timer (HasTimer(..))
import qualified CrossyToad.Time.Timer as Timer
import           CrossyToad.Sprite.AnimationFrame (AnimationFrame)
import qualified CrossyToad.Sprite.AnimationFrame as AnimationFrame
import           CrossyToad.Sprite.Animation as Animation

frame1, frame2 :: AnimationFrame
frame1 = AnimationFrame.mk (V2 0  0) (V2 64 64) 0.5
frame2 = AnimationFrame.mk (V2 64 0) (V2 64 64) 0.5

spec_Sprite_AnimationSpec :: Spec
spec_Sprite_AnimationSpec = do
  describe "mk" $ do
    it "should start in the Paused state" $ do
      let anim = Animation.mk [frame1, frame2]
      anim ^. state `shouldBe` Paused

    it "should focus on the first frame" $ do
      let anim = Animation.mk [frame1, frame2]
      anim ^. currentFrame `shouldBe` frame1

  describe "stepBy" $ do
    it "should do nothing if the animation is paused" $ do
      let anim = Animation.mk [frame1, frame2] & Animation.pause
      stepBy 0.5 anim `shouldBe` anim

    it "should move to the next frame if the animation is playing and the current frame is finished" $ do
      let frame = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.1
      let anim = Animation.mk [frame, frame2] & Animation.play
      (stepBy 1.1 anim) ^. currentFrame . clip `shouldBe` frame2 ^. clip

    it "should move to the next frame if the animation is looping and the current frame is finished" $ do
      let f1 = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.0
      let f2 = AnimationFrame.mk (V2 1 0) (V2 64 64) 1.0
      let anim = Animation.mk [f1, f2] & Animation.loop
      (stepBy 1.1 anim) ^. currentFrame `shouldBe` f2

    it "should stop at the last frame if the animation is playing, and the current frame is finished and the last" $ do
      let f1 = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.0
      let f2 = AnimationFrame.mk (V2 1 0) (V2 64 64) 1.0
      let anim = Animation.mk [f1, f2] & Animation.play
      let nextAnim = (stepBy 1.0 $ stepBy 1.0 anim)
      nextAnim ^. currentFrame . clip `shouldBe` f2 ^. clip

    it "should loop to the first frame if the animation is looping, the current frame is finished and the last" $ do
      let f1 = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.0
      let f2 = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.0
      let anim = Animation.mk [f1, f2] & Animation.loop
      let nextAnim = (stepBy 1.0 $ stepBy 1.0 anim)
      nextAnim ^. currentFrame . clip `shouldBe` f1 ^. clip

  describe "animate" $ do
    it "should switch to the first frame of the animation" $ do
      let f1 = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.0
      let f2 = AnimationFrame.mk (V2 0 0) (V2 64 64) 1.0
      let anim = Animation.mk [f1, f2] & Animation.loop
      let anim' = (stepBy 1.0 $ stepBy 1.0 anim)
      let anim'' = Animation.animate anim'
      anim'' ^. currentFrame . clip `shouldBe` f1 ^. clip

    it "should start the timer of the animation" $ do
      let anim = Animation.mk [frame1] & Animation.loop
      let nextAnim = Animation.animate anim
      (Timer.running $ nextAnim^.currentFrame.timer) `shouldBe` True

  describe "play" $ do
    it "should set the state to playing" $ do
      let anim = Animation.mk [frame1] & Animation.loop
      (Animation.play anim) ^. state `shouldBe` Playing

  describe "loop" $ do
    it "should set the state to looping" $ do
      let anim = Animation.mk [frame1] & Animation.loop
      (Animation.loop anim) ^. state `shouldBe` Looping

  describe "pause" $ do
    it "should set the state to paused" $ do
      let anim = Animation.mk [frame1] & Animation.loop
      (Animation.pause anim) ^. state `shouldBe` Paused
