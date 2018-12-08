{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Geometry.AABBSpec where

import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Geometry.AABB

spec_Physics_AABBSpec :: Spec
spec_Physics_AABBSpec = do
  describe "offset" $ do
    it "should not move when given the identity offset" $ do
      let box = mk (V2 10 10)
      offset (V2 0 0) box `shouldBe` box

    it "should shift the box by the given offset" $ do
      let box = mk (V2 10 10)
      let expected = mkOffset (V2 10 10) (V2 5 5)
      offset (V2 5 5) box `shouldBe` expected

  describe "collision" $ do
    it "should return true if the boxes are overlapping" $ do
      let b1 = mk (V2 5 5)
      let b2 = mk (V2 10 10)
      overlapping b1 b2 `shouldBe` True

    it "should return false if the boxes are only overlapping on an edge" $ do
      let b1 = mkOffset (V2 0 0) (V2 5 5)
      let b2 = mkOffset (V2 5 0) (V2 5 5)
      overlapping b1 b2 `shouldBe` False

    it "should return false if the boxes are not overlapping" $ do
      let b1 = mkOffset (V2 0 0) (V2 5 5)
      let b2 = mkOffset (V2 6 0) (V2 5 5)
      overlapping b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the south" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 0 6) b1
      overlapping b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the north" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 0 (-6)) b1
      overlapping b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the east" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 6 0) b1
      overlapping b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the west" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 (-6) 0) b1
      overlapping b1 b2 `shouldBe` False
