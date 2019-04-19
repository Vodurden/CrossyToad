{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.PhysicalSpec where

import Control.Lens
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Geometry.Position (Position, HasPosition(..))
import CrossyToad.Physics.Physical (Physical, HasPhysical(..), Layer(..))
import qualified CrossyToad.Physics.Physical as Physical

data Ent = Ent
  { __position :: Position
  , __physical :: Physical
  } deriving (Eq, Show)

makeClassy ''Ent

instance HasPosition Ent where position = _position
instance HasPhysical Ent where physical = _physical

-- | TODO: More tests, maybe use Validity
spec_Physics_PhysicalSpec :: Spec
spec_Physics_PhysicalSpec = do
  describe "overlapping" $ do
    it "should return true if the entities are overlapping" $ do
      let ent1 = Ent { __position = (V2 0 0), __physical = Physical.mk (V2 10 10) Ground }
      let ent2 = Ent { __position = (V2 9 9), __physical = Physical.mk (V2 10 10) Ground }
      Physical.overlapping ent1 ent2 `shouldBe` True

    it "should return true if the entities are overlapping on a different layer" $ do
      let ent1 = Ent { __position = (V2 0 0), __physical = Physical.mk (V2 10 10) Ground }
      let ent2 = Ent { __position = (V2 9 9), __physical = Physical.mk (V2 10 10) Air }
      Physical.overlapping ent1 ent2 `shouldBe` True

  describe "colliding" $ do
    it "should return true if the entities are colliding" $ do
      let ent1 = Ent { __position = (V2 0 0), __physical = Physical.mk (V2 10 10) Ground }
      let ent2 = Ent { __position = (V2 9 9), __physical = Physical.mk (V2 10 10) Ground }
      Physical.colliding ent1 ent2 `shouldBe` True

    it "should return false if the entities are colliding on a different layer" $ do
      let ent1 = Ent { __position = (V2 0 0), __physical = Physical.mk (V2 10 10) Ground }
      let ent2 = Ent { __position = (V2 9 9), __physical = Physical.mk (V2 10 10) Air }
      Physical.colliding ent1 ent2 `shouldBe` False
