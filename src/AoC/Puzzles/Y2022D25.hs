module AoC.Puzzles.Y2022D25 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (some)

parse :: String -> Maybe [[Snafu]]
parse = parseMaybe (sepEndBy1 (some snafuDigitP) newline)

solveA :: [[Snafu]] -> String
solveA = concatMap ppSnafu . dec2snafu . sum . map snafu2dec

solveB :: [[Snafu]] -> ()
solveB _ = ()

snafu2dec :: [Snafu] -> Int
snafu2dec = foldl' (\n snafu -> 5 * n + (fromEnum snafu - 2)) 0

dec2snafu :: Int -> [Snafu]
dec2snafu 0 = [Zero]
dec2snafu n =
  let go :: Int -> [Int] -> [Int]
      go 0 ds = ds
      go i ds = go ((i + 2) `div` 5) ((i + 2) `mod` 5 : ds)
   in map toEnum (go n [])

data Snafu = MTwo | MOne | Zero | POne | PTwo
  deriving stock (Show, Eq, Ord, Bounded, Enum)

snafuDigitP :: Parser Snafu
snafuDigitP = enumParser ppSnafu parseSnafu

parseSnafu :: String -> Maybe Snafu
parseSnafu = \case
  "=" -> Just MTwo
  "-" -> Just MOne
  "0" -> Just Zero
  "1" -> Just POne
  "2" -> Just PTwo
  _ -> Nothing

ppSnafu :: Snafu -> String
ppSnafu = \case
  MTwo -> "="
  MOne -> "-"
  Zero -> "0"
  POne -> "1"
  PTwo -> "2"
