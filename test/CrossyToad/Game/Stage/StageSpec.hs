module CrossyToad.Game.Stage.StageSpec where

import           Control.Lens
import qualified Data.Map.Strict as Map

import           CrossyToad.Stage.Entity
import           CrossyToad.Stage.Stage (Stage, HasStage(..))
import qualified CrossyToad.Stage.Stage as Stage
import           CrossyToad.Stage.StageRow (StageRow(..))
import           CrossyToad.Stage.StageFile (StageFile(..))
import qualified CrossyToad.Stage.GroundType as GroundType
import qualified CrossyToad.Physics.Direction as Direction
import qualified CrossyToad.Geometry.Position as Position

import           Test.Tasty.Hspec

spec_Game_Stage_StageSpec :: Spec
spec_Game_Stage_StageSpec =
  describe "fromStageFile" $ do
    it "should load entities in the correct positions" $ do
      let stage' = stageWithEnts [NoEntity, Car, NoEntity, Croc, Croc]

      let expectedEnts = Map.fromList
            [ (Position.fromGrid 0 0, NoEntity)
            , (Position.fromGrid 1 0, Car)
            , (Position.fromGrid 2 0, NoEntity)
            , (Position.fromGrid 3 0, Croc)
            , (Position.fromGrid 6 0, Croc)
            ]

      (view _1) <$> (stage' ^. entities) `shouldBe` expectedEnts

stageWithEnts :: [Entity] -> Stage
stageWithEnts entities' =
  let
    stageRow = StageRow entities' GroundType.Grass Direction.North 0.0
    stageFile = StageFile [stageRow]
  in Stage.fromStageFile stageFile
