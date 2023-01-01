module AoC.Puzzles.Y2022D10 where

import AoC.Lib.Parser
import AoC.Lib.Prelude hiding (cycle)
import Data.Set qualified as Set

parse :: String -> Maybe [Cmd]
parse = parseMaybe (sepEndBy1 (noopP <|> addP) newline)

solveA :: [Cmd] -> Int
solveA cmds = sum (runComp comp0 cmds).singals

solveB :: [Cmd] -> String
solveB cmds = ocr (drawCRT (runComp comp0 cmds).crt)

drawCRT, ocr :: String -> String
drawCRT = intercalate "\n" . chunksOf 40 . reverse
ocr = fromMaybe "" . asciiMapToLetters (Set.singleton '#')

data Cmd
  = Add Int -- 2 cycles
  | Noop -- 1 cycle
  deriving stock (Show)

data Status = Idle | Adding
  deriving stock (Show)

data Comp = Comp
  { regX :: Int,
    cycle :: Int,
    status :: Status,
    singals :: [Int],
    crt :: String
  }
  deriving stock (Show, Generic)

comp0 :: Comp
comp0 = Comp 1 0 Idle [] ""

runComp :: Comp -> [Cmd] -> Comp
runComp comp = flip execState comp . traverse execCmd

execCmd :: Cmd -> State Comp ()
execCmd cmd = do
  #cycle += 1
  regX <- use #regX
  cycle <- use #cycle
  let sample, visible :: Int -> Bool
      sample c = c == 20 || (c - 20) `mod` 40 == 0
      visible x = ((cycle - 1) `mod` 40) `elem` [x - 1, x, x + 1]
  #singals %= \xs -> if sample cycle then regX * cycle : xs else xs
  #crt %= ((if visible regX then '#' else '.') :)
  status <- use #status
  case (cmd, status) of
    (Add i, Adding) -> #status .= Idle >> #regX += i
    (Add _, Idle) -> #status .= Adding >> execCmd cmd
    (Noop, _) -> pure ()

noopP :: Parser Cmd
noopP = strP "noop" >> pure Noop

addP :: Parser Cmd
addP = Add <$> (strP "addx" *> signedIntP)
