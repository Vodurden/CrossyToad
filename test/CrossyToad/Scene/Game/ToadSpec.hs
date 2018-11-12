module CrossyToad.Scene.Game.ToadSpec where

import Control.Lens
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Physics.Physics
import CrossyToad.Scene.Game.Toad as Toad

spec_Scene_Toad :: Spec
spec_Scene_Toad = do
  describe "die" $ do
    it "should reduce the toads lives by 1" $ do
      let toad' = (Toad.mk $ V2 0 0) & (lives .~ 2)
      (Toad.die toad') ^. lives `shouldBe` 1

    it "should not reduce the toads lives below 0" $ do
      let toad' = (Toad.mk $ V2 0 0) & (lives .~ 0)
      (Toad.die toad') ^. lives `shouldBe` 0

    it "should reset the toad to it's starting position" $ do
      let toad' = Toad.mk $ V2 64 128
      let movedToad = toad' & (position .~ (V2 0 0))
      (Toad.die movedToad) ^. position `shouldBe` (toad' ^. position)

  describe "collision" $ do
    it "should return true if the toad is colliding with the entity" $ do
      let toad' = (Toad.mk $ V2 0 0)
      let toad2 = (Toad.mk (V2 10 10))
      Toad.collision toad' toad2 `shouldBe` True

    it "should return false if the toad is jumping when colliding" $ do
      let jumpingToad = Toad.jump North (Toad.mk $ V2 0 0)
      let toad2 = (Toad.mk (V2 10 10))
      Toad.collision jumpingToad toad2 `shouldBe` False
