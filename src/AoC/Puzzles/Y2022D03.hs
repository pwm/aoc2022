module AoC.Puzzles.Y2022D03 where

import AoC.Prelude
import Data.Map.Strict qualified as Map

parse :: String -> Maybe [String]
parse = Just . lines

solveA :: [String] -> Int
solveA = sum . map (commonLetterVal . splitMiddle)

solveB :: [String] -> Int
solveB = sum . map commonLetterVal . chunksOf 3

splitMiddle :: [a] -> [[a]]
splitMiddle l = t2l $ splitAt (length l `div` 2) l

commonLetterVal :: [String] -> Int
commonLetterVal = fromMaybe 0 . (letterMap !?) . head . nubOrd . intersections

letterMap :: Map Char Int
letterMap =
  let letters = ['a' .. 'z'] <> ['A' .. 'Z']
   in Map.fromList $ zip letters [1 ..]
