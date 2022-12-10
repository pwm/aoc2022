module AoC.Puzzles.Y2022D06 where

import AoC.Lib.Prelude
import Control.Monad.State.Strict

parse :: String -> Maybe String
parse = Just

solveA :: String -> Int
solveA = solveFor 4

solveB :: String -> Int
solveB = solveFor 14

solveFor :: Int -> String -> Int
solveFor markerLength = flip evalState (1, "") . go
  where
    go :: String -> State (Int, String) Int
    go stream = do
      (pos, marker) <- get
      let (cur, rest) = splitAt 1 stream
          marker' = marker <> cur
      if
          | marker' == nubOrd marker' && length marker' == markerLength -> pure pos
          | marker' == nubOrd marker' -> put (pos + 1, marker') >> go rest
          | otherwise -> put (pos + 1, drop 1 marker') >> go rest
