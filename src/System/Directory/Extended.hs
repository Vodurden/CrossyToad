module System.Directory.Extended
  ( module System.Directory
  , listDirectoryPaths
  ) where

import System.Directory
import System.FilePath

-- | Equivalent to listDirectory but the returned paths are
-- | full paths instead of just file names
listDirectoryPaths :: FilePath -> IO [FilePath]
listDirectoryPaths dirPath = (fmap . fmap) (dirPath </>) (listDirectory dirPath)
