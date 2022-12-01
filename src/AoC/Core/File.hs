module AoC.Core.File
  ( readInput,
    inputPath,
    inputName,
  )
where

import AoC.Core.Date
import System.Environment (getEnv)
import System.FilePath (normalise)
import System.IO (readFile')
import Prelude

readInput :: Date -> IO String
readInput date = inputPath date >>= readFile'

inputPath :: Date -> IO FilePath
inputPath date = do
  inputDirPath <- getEnv "AOC_INPUT_PATH"
  pure $ normalise (inputDirPath <> "/" <> inputName date)

inputName :: Date -> String
inputName date = displayDate date <> ".txt"
