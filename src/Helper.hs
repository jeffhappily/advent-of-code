module Helper (readInputFile) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

readInputFile :: String -> IO String
readInputFile fileName = do
  currentDir <- getCurrentDirectory
  let filePath = currentDir </> fileName
  readFile filePath