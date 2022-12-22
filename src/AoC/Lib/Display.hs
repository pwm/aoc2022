module AoC.Lib.Display where

import AoC.Lib.Grid
import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import System.IO qualified as SIO

displayGrid :: GridOf cell -> (cell -> String) -> IO ()
displayGrid grid drawCell = do
  SIO.hSetBuffering stdin SIO.NoBuffering
  SIO.hSetEcho stdin False
  putStrLn clrScr
  putStrLn $ drawGrid drawCell grid

-- Draw by moving the cursor to the cell pos, this works with eg. holey grids
-- The resulting string will look sg. like "\ESC[12;16H.\ESC[12;15H# ..."
drawGrid :: forall cell. (cell -> String) -> GridOf cell -> String
drawGrid drawCell =
  let putCell :: Pos -> cell -> String -> String
      putCell pos a screen = screen <> cursorTo pos <> drawCell a
   in Map.foldrWithKey putCell ""

clrScr :: String
clrScr = "\ESC[2J"

cursorTo :: Pos -> String
cursorTo (x, y) = "\ESC[" <> show (x + 1) <> ";" <> show (y + 1) <> "H"

-- Colour

colourAs :: Colour -> String -> String
colourAs c s = "\ESC[" <> colourToAnsi c <> ";1m" <> s <> "\ESC[0m"

data Colour
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Reset
  deriving stock (Show, Eq, Ord)

colourToAnsi :: Colour -> String
colourToAnsi = \case
  Black -> "30"
  Red -> "31"
  Green -> "32"
  Yellow -> "33"
  Blue -> "34"
  Magenta -> "35"
  Cyan -> "36"
  White -> "70"
  Reset -> "0"

-- Interaction

getKey :: IO String
getKey = reverse <$> getKeyPress ""
  where
    getKeyPress :: String -> IO String
    getKeyPress chars = do
      char <- SIO.getChar
      let chars' = char : chars
      more <- SIO.hReady stdin
      if more then getKeyPress chars' else pure chars'
