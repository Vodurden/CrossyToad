{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module CrossyToad.Physics.JumpMotionSpec where

import           Control.Lens
import           Linear.V2

import           Test.Tasty.Hspec

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Distance
import           CrossyToad.Physics.Speed
import           CrossyToad.Time.Seconds

data Ent = Ent
  { __position :: !Position
  , __direction :: !Direction
  , __jumpMotion :: !JumpMotion
  } deriving (Eq, Show)

makeClassy ''Ent

instance HasPosition Ent where
  position = _position

instance HasDirection Ent where
  direction = _direction

instance HasJumpMotion Ent where
  jumpMotion = _jumpMotion

mkEnt :: Direction -> Speed -> Distance -> Seconds -> Ent
mkEnt direction' speed' distance' cooldown' = Ent
  { __position = V2 0 0
  , __direction = direction'
  , __jumpMotion = JumpMotion.mk speed' distance' cooldown'
  }

mkEnt' :: Ent
mkEnt' = mkEnt East 1 64 0.15

ready :: Ent -> Ent
ready = id

jumping :: Ent -> Ent
jumping ent' = JumpMotion.jump (ent' ^. direction) $ ent'

coolingDown :: Ent -> Ent
coolingDown ent' = JumpMotion.tick (ent'^.jumpMotion.speed * ent'^.jumpMotion.distance) $ jumping ent'

spec_Physics_JumpMotion :: Spec
spec_Physics_JumpMotion = do
  describe "jump" $ do
    it "should not jump if we are already jumping" $ do
      let ent' = jumping $ mkEnt' & (direction .~ East)
      (jump West ent') `shouldBe` ent'

    it "should not jump if we are cooling down" $ do
      let ent' = coolingDown mkEnt'
      (jump East ent') `shouldBe` ent'

    it "should begin jumping if we are Ready" $ do
      let ent' = ready mkEnt'
      JumpMotion.isJumping (jump East ent') `shouldBe` True

    it "should update the direction if we are not jumping" $ do
      let ent' = mkEnt' & (direction .~ East)
      (jump West ent') ^. direction `shouldBe` West

  describe "isJumping" $ do
    it "should be true when we are jumping" $
      isJumping (jumping mkEnt') `shouldBe` True

    it "should be false when we are ready" $
      isJumping (ready mkEnt') `shouldBe` False

    it "should be false when we are cooling down" $
      isJumping (coolingDown mkEnt') `shouldBe` False

  describe "tick" $ do
    let tick' = tick 1

    context "when the entity is ready it" $ do
      it "should not do anything" $ do
        let ent' = ready mkEnt'
        (tick' ent') `shouldBe` ent'

    context "when the entity is jumping it" $ do
      it "should not move further then the target distance" $ do
        let ent' = jumping $ mkEnt East 2 1 0.15
        (tick' ent') ^. position `shouldBe` (V2 1 0)

      it "moves by the speed of the motion" $ do
        let ent' = jumping $ mkEnt East 2 5 0.15
        (tick' ent') ^. position `shouldBe` (V2 2 0)

      it "does nothing when the speed is 0" $ do
        let ent' = jumping $ mkEnt' & (jumpMotion.speed .~ 0)
        tick' ent' `shouldBe` ent'

      it "does not move further then the target distance" $ do
        let ent' = jumping $ mkEnt East 10 5 0.15
        (tick' ent') ^. position `shouldBe` (V2 5 0)

      it "should move linearly relative to the delta time" $ do
        let ent' = jumping $ mkEnt East 10 10 0.15
        let delta = 0.1
        (tick delta ent') ^. position `shouldBe` (V2 1 0)

      it "does not move further then the target distance when linearized" $ do
        let ent' = jumping $ mkEnt East 20 1 0.15
        let delta = 0.1
        (tick delta ent') ^. position `shouldBe` (V2 1 0)

      it "should not linearize target distance" $ do
        let ent' = jumping $ mkEnt East 320 32 0.15
        let delta = 0.05
        (tick delta ent') ^. position `shouldBe` (V2 16 0)

    context "when the entity is cooling down it" $ do
      it "should not move" $ do
        let ent' = coolingDown $ mkEnt'
        (tick' ent') ^. position `shouldBe` (ent' ^. position)

      it "should become ready when the cooldown finishes" $ do
        let ent' = coolingDown $ mkEnt'
        let cooldown' = ent' ^. cooldown
        JumpMotion.isReady (tick cooldown' ent') `shouldBe` True

    context "when the entity finishes jumping" $ do
      it "should begin cooling down" $ do
        let ent' = jumping $ mkEnt East 5 5 0.15
        JumpMotion.isCoolingDown (tick' ent') `shouldBe` True
