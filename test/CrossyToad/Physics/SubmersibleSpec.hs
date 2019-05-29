{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.SubmersibleSpec where

import Control.Lens

import Test.Tasty.Hspec

import           CrossyToad.Physics.Submersible (Submersible, HasSubmersible(..))
import qualified CrossyToad.Physics.Submersible as Submersible

data Ent = Ent
  { __submersible :: Submersible
  }

makeClassy ''Ent

instance HasSubmersible Ent where submersible = _submersible

spec_Physics_SubmersibleSpec :: Spec
spec_Physics_SubmersibleSpec = do
  describe "tick" $ do
    it "should sink if we are swimming for too long" $ do
      let ent' = Ent (Submersible.mk 2 1)
      let tickEnt' = Submersible.tick 3 ent'
      (Submersible.sunk tickEnt') `shouldBe` True

    it "should stay swimming if we are swimming for too long" $ do
      let ent' = Ent (Submersible.mk 2 1)
      let tickEnt' = Submersible.tick 1 ent'
      (Submersible.swimming tickEnt') `shouldBe` True

    it "should swim if we are sunk for too long" $ do
      let ent' = Ent (Submersible.mk 2 1)
      let tickEnt' = Submersible.tick 3 ent'
      (Submersible.sunk tickEnt') `shouldBe` True

  describe "progress" $ do
    it "should give be 0.0 if we haven't ticked at all" $ do
      let ent' = Ent (Submersible.mk 2 1)
