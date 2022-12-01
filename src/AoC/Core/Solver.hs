module AoC.Core.Solver
  ( Solutions,
    solvePuzzle,
    mkSolverFor,
  )
where

import AoC.Core.Date
import AoC.Core.File
import Data.Map.Strict (Map, (!?))
import System.Exit (exitFailure, exitSuccess)
import Prelude

type Solutions = Map (Int, Int) (Date -> IO ())

solvePuzzle :: Solutions -> Date -> IO ()
solvePuzzle solutions date =
  case solutions !? (getYear date.year, getDay date.day) of
    Nothing -> print ("Cannot find solution for " <> inputName date) >> exitFailure
    Just solverFor -> solverFor date

mkSolverFor ::
  (Show a, Show b) =>
  (String -> Maybe i) ->
  (i -> a) ->
  (i -> b) ->
  (Date -> IO ())
mkSolverFor parse solveA solveB date = do
  inputFile <- readInput date
  case parse inputFile of
    Nothing -> print ("Cannot parse " <> inputName date) >> exitFailure
    Just input -> print (solveA input, solveB input) >> exitSuccess
