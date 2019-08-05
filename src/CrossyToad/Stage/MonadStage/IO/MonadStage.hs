module CrossyToad.Stage.MonadStage.IO.MonadStage
  ( stages
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as Text
import           Data.Either (partitionEithers)
import           Data.Foldable (traverse_)
import           System.Directory.Extended

import           CrossyToad.Stage.StageFile (StageFile)
import qualified CrossyToad.Stage.StageFile as StageFile
import           CrossyToad.Stage.Stage (Stage)
import qualified CrossyToad.Stage.Stage as Stage

stages :: MonadIO m => m [Stage]
stages = (fmap . fmap) Stage.fromStageFile stageFiles

stageFiles :: MonadIO m => m [StageFile]
stageFiles = do
  stageFilePaths <- liftIO $ listDirectoryPaths "./assets/levels/"

  stageFilesOrErrors <- liftIO $ (flip traverse) stageFilePaths $ \stageFilePath -> do
    stageFile <- Text.readFile stageFilePath
    pure $ StageFile.load stageFilePath stageFile

  let (errors, loadedStages) = partitionEithers stageFilesOrErrors

  liftIO $ (flip traverse_) errors $ \error' -> do
    putStrLn "[ERROR] Failed to load level!"
    putStrLn error'

  pure loadedStages
