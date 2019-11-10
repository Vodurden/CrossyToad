module CrossyToad.Victory.MonadHighScore.IO.MonadHighScore
  ( highScores
  , saveScore
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.IO as Text
import           System.Directory (doesFileExist)

import           CrossyToad.Victory.HighScore (HighScore)
import           CrossyToad.Victory.HighScores (HighScores)
import qualified CrossyToad.Victory.HighScores as HighScores

highScoresFilePath :: String
highScoresFilePath = "high-scores"

highScores :: MonadIO m => m [HighScore]
highScores = do
  highScoresOrError <- liftIO loadHighScoresFile
  case highScoresOrError of
    Left(err) -> do
      liftIO $ putStrLn "[ERROR] Failed to load High Scores!"
      liftIO $ putStrLn err
      pure []
    Right(highScores') ->
      pure $ HighScores.toList highScores'

saveScore :: MonadIO m => HighScore -> m ()
saveScore score' = do
  highScoresOrError <- loadHighScoresFile
  case highScoresOrError of
    Left(err) -> do
      liftIO $ putStrLn "[ERROR] Failed to load High Scores!"
      liftIO $ putStrLn "[ERROR] Your high score was not saved"
      liftIO $ putStrLn err
    Right(highScores') ->
      saveHighScoresFile (HighScores.addScore score' highScores')

loadHighScoresFile :: MonadIO m => m (Either String HighScores)
loadHighScoresFile = do
  highScoreFileExists <- liftIO $ doesFileExist highScoresFilePath
  if highScoreFileExists
  then do
    highScoresFile <- liftIO $ Text.readFile highScoresFilePath
    pure $ HighScores.load highScoresFilePath highScoresFile
  else pure $ Right HighScores.empty

saveHighScoresFile :: MonadIO m => HighScores -> m ()
saveHighScoresFile highScores' =
  liftIO $ Text.writeFile highScoresFilePath (HighScores.save highScores')
