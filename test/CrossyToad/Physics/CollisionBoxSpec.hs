{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.CollisionBoxSpec where

import Control.Lens
import Linear.V2

import Test.Tasty.Hspec

import CrossyToad.Geometry.Geometry
import CrossyToad.Geometry.Position
import CrossyToad.Physics.CollisionBox

data Ent = Ent
  { __position :: !(Position 'World)
  , __collisionBox :: !CollisionBox
  } deriving (Eq, Show)

makeClassy ''Ent

instance HasPosition Ent 'World where
  position = _position

instance HasCollisionBox Ent where
  collisionBox = _collisionBox

spec_Physics_CollisionBoxSpec :: Spec
spec_Physics_CollisionBoxSpec = do
  describe "entCollision" $ do
    it "should offset both boxes by the entities position" $ do
      let ent1 = Ent { __position = (V2 0 0), __collisionBox = mk (V2 10 10) }
      let ent2 = Ent { __position = (V2 15 15), __collisionBox = mk (V2 10 10) }

      entCollision ent1 ent2 `shouldBe` False

    it "should return true if the entities are overlapping" $ do
      let ent1 = Ent { __position = (V2 0 0), __collisionBox = mk (V2 10 10) }
      let ent2 = Ent { __position = (V2 9 9), __collisionBox = mk (V2 10 10) }
      entCollision ent1 ent2 `shouldBe` True
