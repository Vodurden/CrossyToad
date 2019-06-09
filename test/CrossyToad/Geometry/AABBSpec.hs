module CrossyToad.Geometry.AABBSpec where

import Control.Lens
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Geometry.Position
import CrossyToad.Geometry.Size
import CrossyToad.Geometry.AABB


spec_Geometry_AABB :: Spec
spec_Geometry_AABB = do
  describe "position" $ do
    it "should be the min point" $ do
      let box = AABB (V2 10 10) (V2 20 20)
      box ^. position `shouldBe` (V2 10 10)

  describe "size" $ do
    it "should be the width and height of the AABB" $ do
      let box = AABB (V2 10 10) (V2 50 30)
      box ^. size `shouldBe` (V2 40 20)

  describe "mk" $ do
    it "should make an AABB with the given size" $ do
      let box = mk (V2 50 50)
      box ^. size `shouldBe` (V2 50 50)

  describe "mkAt" $ do
    it "should make an AABB at the position with the size" $ do
      let box = mkAt (V2 15 15) (V2 50 50)
      box ^. position `shouldBe` (V2 15 15)
      box ^. size `shouldBe` (V2 50 50)

  describe "offset" $ do
    it "should not move when given an identity vector" $ do
      let box = mk (V2 10 10)
      offset (V2 0 0) box `shouldBe` box

    it "should shift the box by the given vector" $ do
      let box = mk (V2 10 10)
      let expected = mkAt (V2 5 5) (V2 10 10)
      offset (V2 5 5) box `shouldBe` expected

  describe "shrink" $ do
    it "should shrink the AABB by the given values" $ do
      let box = mk (V2 50 50)
      let expected = AABB (V2 1 3) (V2 48 46)
      shrink 1 2 3 4 box `shouldBe` expected

  describe "shrinkParallel" $ do
    it "should be the same as using shrink" $ do
      let box = mk (V2 50 50)
      let shrinkX = 10
      let shrinkY = 5
      let expected = shrink shrinkX shrinkX shrinkY shrinkY box
      shrinkParallel shrinkX shrinkY box `shouldBe` expected

  describe "collision" $ do
    it "should return true if the boxes are overlapping" $ do
      let b1 = mk (V2 5 5)
      let b2 = mk (V2 10 10)
      collision b1 b2 `shouldBe` True

    it "should return false if the boxes are only overlapping on an edge" $ do
      let b1 = mkAt (V2 0 0) (V2 5 5)
      let b2 = mkAt (V2 5 0) (V2 5 5)
      collision b1 b2 `shouldBe` False

    it "should return false if the boxes are not overlapping" $ do
      let b1 = mkAt (V2 0 0) (V2 5 5)
      let b2 = mkAt (V2 6 0) (V2 5 5)
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the south" $ do
      let b1 = mkAt (V2 5 0) (V2 5 5)
      let b2 = offset (V2 0 6) b1
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the north" $ do
      let b1 = mkAt (V2 5 0) (V2 5 5)
      let b2 = offset (V2 0 (-6)) b1
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the east" $ do
      let b1 = mkAt (V2 5 0) (V2 5 5)
      let b2 = offset (V2 6 0) b1
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the west" $ do
      let b1 = mkAt (V2 5 0) (V2 5 5)
      let b2 = offset (V2 (-6) 0) b1
      collision b1 b2 `shouldBe` False
