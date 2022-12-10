module AoC.Puzzles.Y2022D02 where

import AoC.Lib.Prelude

parse :: String -> Maybe [(Elf, Me)]
parse = traverse (bitraverse parseElf parseMe <=< l2p . splitOn " ") . lines

solveA :: [(Elf, Me)] -> Int
solveA = sum . map (myScore . bimap elfHand myHandA)

solveB :: [(Elf, Me)] -> Int
solveB = sum . map (myScore . \(elf, me) -> (elfHand elf, myHandB elf me))

data Elf = A | B | C
  deriving stock (Show, Eq)

data Me = X | Y | Z
  deriving stock (Show, Eq)

data Hand = Rock | Paper | Scissors
  deriving stock (Show, Eq)

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare Rock Scissors = GT
  compare Rock Paper = LT
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Paper = GT
  compare Scissors Rock = LT
  compare _ _ = EQ

elfHand :: Elf -> Hand
elfHand = \case
  A -> Rock
  B -> Paper
  C -> Scissors

myHandA :: Me -> Hand
myHandA = \case
  X -> Rock
  Y -> Paper
  Z -> Scissors

myHandB :: Elf -> Me -> Hand
myHandB elf = \case
  X -> elfIs (>)
  Y -> elfIs (==)
  Z -> elfIs (<)
  where
    elfIs :: (Hand -> Hand -> Bool) -> Hand
    elfIs comp = head [h | h <- [Rock, Paper, Scissors], elfHand elf `comp` h]

myScore :: (Hand, Hand) -> Int
myScore (elfH, myH) = myRoundScore + myHandScore
  where
    myRoundScore
      | elfH < myH = 6
      | elfH == myH = 3
      | otherwise = 0
    myHandScore = case myH of
      Rock -> 1
      Paper -> 2
      Scissors -> 3

parseElf :: String -> Maybe Elf
parseElf "A" = Just A
parseElf "B" = Just B
parseElf "C" = Just C
parseElf _ = Nothing

parseMe :: String -> Maybe Me
parseMe "X" = Just X
parseMe "Y" = Just Y
parseMe "Z" = Just Z
parseMe _ = Nothing
