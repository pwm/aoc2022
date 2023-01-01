module AoC.Puzzles.Y2022D20 where

import AoC.Lib.Prelude
import Data.CircularList (CList)
import Data.CircularList qualified as CL

parse :: String -> Maybe [Int]
parse = stringToInts

-- 6220 too low
solveA :: [Int] -> ()
solveA xs =
  let cl = mix xs
      res = focusN 1000 cl + focusN 2000 cl + focusN 3000 cl
   in ()

solveB :: a -> ()
solveB _ = ()

mix :: [Int] -> CList Int
mix xs = rotTo 0 $ foldl' (flip rot1) (CL.fromList xs) xs

{-
Initial arrangement:       [1, 2, -3, 3, -2, 0, 4]

1 moves between 2 and -3:  [2, 1, -3, 3, -2, 0, 4]
2 moves between -3 and 3:  [1, -3, 2, 3, -2, 0, 4]
-3 moves between -2 and 0: [1, 2, 3, -2, -3, 0, 4] - bad (off by 1)
3 moves between 0 and 4:   [1, 2, -2, -3, 0, 3, 4]
-2 moves between 4 and 1:  [1, 2, -3, 0, 3, 4, -2] - bad (off by 1)
0 does not move:           [1, 2, -3, 0, 3, 4, -2]
4 moves between -3 and 0:  [1, 2, -3, 4, 0, 3, -2] - bad (off by 1)

-}

rot1 :: Int -> CList Int -> CList Int
rot1 i cl
  | CL.isEmpty cl = CL.empty
  | i == 0 = cl
  | otherwise =
      let !(!cl1, !n) = withIO (p 0 cl) $ rotRToElem i cl
          !cl2 = withIO (p 1 cl1) $ CL.removeR cl1
          !cl3 = withIO (p 2 cl2) $ CL.rotN i cl2 -- can wrap early cause of remove
          !cl4 = withIO (p 3 cl3) $ CL.insertR i cl3
          !cl5 = withIO (p 4 cl4) $ CL.rotN (-i) cl4 -- -i + 1 if wraps
          !cl6 = withIO (p 5 cl5) $ CL.rotN (-n) cl5
       in withIO (p 6 cl6) cl6

p :: Int -> CList Int -> IO ()
p i cl = putStr "" >> putStrLn (show i <> ": " <> show cl)

-- always returns positive n so CL.rotN is fine ie. it'll be rotL
rotRToElem :: Int -> CList Int -> (CList Int, Int)
rotRToElem i =
  let step :: CList Int -> State Int (CList Int)
      step cl = modify (+ 1) >> pure (CL.rotR cl)
   in flip runState 0 . loopTillM (pure . (== i) . focus) step

rotTo :: Int -> CList Int -> CList Int
rotTo i = fromJust . CL.rotateTo i

focusN :: Int -> CList Int -> Int
focusN i = focus . CL.rotN i

focus :: CList Int -> Int
focus = fromJust . CL.focus

--

cl0 :: CList Int
cl0 = CL.fromList l0

l0 :: [Int]
l0 = [1, 2, -3, 3, -2, 0, 4]

--

s0 :: String
s0 =
  unpack
    [trimming|
1
2
-3
3
-2
0
4
|]
