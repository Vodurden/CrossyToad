{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.CollisionBoxSpec where

import Control.Lens
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Physics.Position
import CrossyToad.Physics.CollisionBox

data Ent = Ent
  { __position :: Position
  , __collisionBox :: CollisionBox
  } deriving (Eq, Show)

makeClassy ''Ent

instance HasPosition Ent where
  position = _position

instance HasCollisionBox Ent where
  collisionBox = _collisionBox

spec_Physics_CollisionBoxSpec :: Spec
spec_Physics_CollisionBoxSpec = do
  describe "offset" $ do
    it "should not move when given an identity vector" $ do
      let box = mk (V2 10 10)
      offset (V2 0 0) box `shouldBe` box

    it "should shift the box by the given vector" $ do
      let box = mk (V2 10 10)
      let expected = CollisionBox { _minPoint = (V2 5 5), _maxPoint = (V2 15 15) }
      offset (V2 5 5) box `shouldBe` expected

  describe "entCollision" $ do
    it "should offset both boxes by the entities position" $ do
      let ent1 = Ent { __position = (V2 0 0), __collisionBox = mk (V2 10 10) }
      let ent2 = Ent { __position = (V2 15 15), __collisionBox = mk (V2 10 10) }
      entCollision ent1 ent2 `shouldBe` False

    it "should return true if the entities are overlapping" $ do
      let ent1 = Ent { __position = (V2 0 0), __collisionBox = mk (V2 10 10) }
      let ent2 = Ent { __position = (V2 9 9), __collisionBox = mk (V2 10 10) }
      entCollision ent1 ent2 `shouldBe` True

  describe "collision" $ do
    it "should return true if the boxes are overlapping" $ do
      let b1 = mk (V2 5 5)
      let b2 = mk (V2 10 10)
      collision b1 b2 `shouldBe` True

    it "should return false if the boxes are only overlapping on an edge" $ do
      let b1 = mkOffset (V2 0 0) (V2 5 5)
      let b2 = mkOffset (V2 5 0) (V2 5 5)
      collision b1 b2 `shouldBe` False

    it "should return false if the boxes are not overlapping" $ do
      let b1 = mkOffset (V2 0 0) (V2 5 5)
      let b2 = mkOffset (V2 5.1 0) (V2 5 5)
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the south" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 0 6) b1
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the north" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 0 (-6)) b1
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the east" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 6 0) b1
      collision b1 b2 `shouldBe` False

    it "should return false if the box is not overlapping to the west" $ do
      let b1 = mkOffset (V2 5 0) (V2 5 5)
      let b2 = offset (V2 (-6) 0) b1
      collision b1 b2 `shouldBe` False
