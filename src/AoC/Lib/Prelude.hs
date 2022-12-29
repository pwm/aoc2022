{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module AoC.Lib.Prelude
  ( module X,
    module List,
    module Map,
    module Seq,
    module Set,
    module T,
    load,
    loadDate,
    stringToInt,
    stringToInts,
    stringBlocksToInts,
    stringToIntsSepBy,
    stringToDigits,
    charToDigit,
    integerToDigits,
    digitsToInteger,
    intToDigits,
    digitsToInt,
    pp,
    ppw,
    pps,
    pad,
    rpad,
    withIO,
    fixpoint,
    fixpointL,
    fixpointM,
    loopTill,
    loopTillM,
    headOr,
    lastOr,
    takeEnd,
    dropEnd,
    enumerate,
    hasKeys,
    rsort,
    decoSort,
    charAt,
    l2p,
    l2p3,
    t2l,
    tupleMin,
    tupleMax,
    tupleSum,
    tupleProduct,
    pick,
    slicesOf,
    lookups,
    setLookups,
    rotate,
    compose,
    composeM,
    times,
    timesAcc,
    timesL,
    timesM,
    substring,
    binToDec,
    decToBin,
    sqrtInt,
    choose,
    ichoose,
    unions,
    intersections,
  )
where

import Advent.OCR as X
import AoC.Core.Date
import AoC.Core.File
import Control.Applicative as X (Alternative (..), liftA2, liftA3)
import Control.Lens as X (Each (..), element, filtered, filteredBy, folded, maximumByOf, maximumOf, minimumByOf, minimumOf, over, preview, review, set, sumOf, toListOf, use, uses, view, (%=), (%~), (*~), (+=), (+~), (-~), (.=), (.~), (^.), (^..), (^?), _1, _2, _3, _4, _5, _Just, _Nothing)
import Control.Monad as X (foldM, guard, when, (<=<), (>=>))
import Control.Monad.Logic (MonadLogic, interleave)
import Data.Bifunctor as X
import Data.Bitraversable as X
import Data.Char as X (chr, ord)
import Data.Containers.ListUtils as X
import Data.Either as X
import Data.Foldable as X (Foldable (..), asum, traverse_)
import Data.Function as X
import Data.Functor as X
import Data.Functor.Identity as X (Identity (..))
import Data.Generics.Labels as X ()
import Data.Kind as X
import Data.List as List
import Data.List.Split as X
import Data.Map.Strict as Map (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe as X
import Data.Ord as X (Down (..), comparing)
import Data.Sequence as Seq (Seq)
import Data.Set as Set (Set)
import Data.Set qualified as Set
import Data.Text as T (Text, unpack)
import Data.Text.Lazy as TL (unpack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Tuple as X
import Data.Void as X (Void)
import Debug.Trace as X
import GHC.Generics as X (Generic)
import NeatInterpolation as X hiding (text)
import System.IO as X (stdin)
import System.IO.Unsafe as X (unsafePerformIO)
import Text.Pretty.Simple (CheckColorTty (..), OutputOptions (..), StringOutputStyle (..), pPrintOpt, pShowOpt)
import Text.Read (readMaybe)
import Prelude as X

load :: IO String
load = getDate >>= readInput
  where
    getDate :: IO Date
    getDate = do
      x <- getCurrentTime
      let (y, _, d) = toGregorian $ utctDay x
      case mkDate (fromIntegral y) d of
        (Right date) -> pure date
        _ -> error "Not a valid date"

loadDate :: Int -> Int -> IO String
loadDate y d = case mkDate y d of
  (Right (MkDate year day)) -> readInput (MkDate year day)
  _ -> error "Cannot read file"

-- "123456" -> Just 123456
stringToInt :: String -> Maybe Int
stringToInt = readMaybe

-- "123\n456" -> Just [123, 456]
stringToInts :: String -> Maybe [Int]
stringToInts = traverse stringToInt . lines

-- "12\n34\n\n56\n78\n" -> Just [[12,34],[56,78]]
stringBlocksToInts :: String -> Maybe [[Int]]
stringBlocksToInts = traverse (traverse stringToInt) . splitOn [""] . lines

-- "12,34,56" -> Just [12,34,56]
-- "12-34" -> Just [12,34]
stringToIntsSepBy :: String -> String -> Maybe [Int]
stringToIntsSepBy sep = traverse stringToInt . splitOn sep

-- "123456" -> Just [1, 2, 3, 4, 5, 6]
stringToDigits :: String -> Maybe [Int]
stringToDigits s =
  let xs = concatMap (map fst . (\c -> reads @Int [c])) s
   in if length xs == length s then Just xs else Nothing

-- '1' -> Just 1
charToDigit :: Char -> Maybe Int
charToDigit c = case reads @Int [c] of
  [(n, "")] -> Just n
  _ -> Nothing

-- 0 -> [0]
-- (-123456) -> [1, 2, 3, 4, 5, 6]
integerToDigits :: Integer -> [Int]
integerToDigits 0 = [0]
integerToDigits i = reverse $ unfoldr go (abs i)
  where
    go :: Integer -> Maybe (Int, Integer)
    go 0 = Nothing
    go n = Just (fromInteger @Int (n `mod` 10), n `div` 10)

-- [1, 2, 3, 4, 5, 6] -> Just 123456
-- [] -> Nothing
digitsToInteger :: [Int] -> Maybe Integer
digitsToInteger [] = Nothing
digitsToInteger xs = Just $ foldl' (\i d -> i * 10 + toInteger d) 0 xs

intToDigits :: Int -> [Int]
intToDigits = integerToDigits . fromIntegral

digitsToInt :: [Int] -> Maybe Int
digitsToInt = fmap fromIntegral . digitsToInteger

pp :: (Show a) => a -> IO ()
pp = pPrintOpt NoCheckColorTty outOpts

ppw :: (Show a) => Int -> a -> IO ()
ppw width = pPrintOpt NoCheckColorTty (outOpts & #outputOptionsPageWidth .~ width)

pps :: (Show a) => a -> String
pps = TL.unpack . pShowOpt outOpts

outOpts :: OutputOptions
outOpts =
  OutputOptions
    { outputOptionsIndentAmount = 2,
      outputOptionsPageWidth = 80,
      outputOptionsCompact = True,
      outputOptionsCompactParens = True,
      outputOptionsInitialIndent = 0,
      outputOptionsColorOptions = Nothing,
      outputOptionsStringStyle = EscapeNonPrintable
    }

pad :: Int -> String -> String
pad n s
  | length s >= n = s
  | otherwise = s <> replicate (n - length s) ' '

rpad :: Int -> String -> String
rpad n s
  | length s >= n = s
  | otherwise = replicate (n - length s) ' ' <> s

withIO :: IO a -> b -> b
withIO a b = let !_ = unsafePerformIO a in b

-- strict
fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x | y <- f x = if y `seq` x == y then y else fixpoint f y

-- lazy
fixpointL :: (Eq a) => (a -> a) -> a -> a
fixpointL f x = if x == f x then x else fixpoint f (f x)

fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f x = do
  y <- f x
  if x == y then pure y else fixpointM f y

compose :: (Foldable t) => t (b -> b) -> b -> b
compose = foldr (.) id

composeM :: (Foldable t, Monad m) => t (b -> m b) -> b -> m b
composeM = foldr (<=<) pure

-- strict
times :: Int -> (b -> b) -> b -> b
times n f s = foldl' (\x _ -> f x) s (replicate n ())

timesAcc :: Int -> (b -> b) -> b -> [b]
timesAcc n f s = scanl' (\x _ -> f x) s (replicate n ())

-- lazy
timesL :: Int -> (b -> b) -> b -> b
timesL n = compose . replicate n

timesM :: (Monad m) => Int -> (b -> m b) -> b -> m b
timesM n = composeM . replicate n

loopTill :: (a -> Bool) -> (a -> a) -> a -> a
loopTill p step x = if p x then x else loopTill p step (step x)

loopTillM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
loopTillM p step x = do
  b <- p x
  if b then pure x else step x >>= loopTillM p step

-- headOr 0 [] -> 0
-- headOr 0 [1, 2, 3] -> 1
headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (a : _) = a

-- lastOr 0 [] -> 0
-- lastOr 0 [1, 2, 3] -> 3
lastOr :: a -> [a] -> a
lastOr x [] = x
lastOr _ [x] = x
lastOr x (_ : xs) = lastOr x xs

-- takeEnd 1 [1..3] -> [1, 2]
takeEnd :: Int -> [a] -> [a]
takeEnd n xs = drop (length xs - n) xs

-- dropEnd 1 [1..3] -> [1, 2]
dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

-- enumerate @Bool -> [False,True]
enumerate :: forall a. (Bounded a, Enum a) => [a]
enumerate = enumFrom (minBound @a)

-- {a, b} -> [(a, 1), (b, 2), (c, 3)] -> True
-- {a, b, c} -> [(a, 1), (b, 2)] -> False
hasKeys :: (Ord a) => Set a -> Map a b -> Bool
hasKeys keys = Set.isSubsetOf keys . Map.keysSet

-- [2,3,1,2] -> [3,2,2,1]
rsort :: (Ord a) => [a] -> [a]
rsort = sortOn Down

decoSort :: (Ord b) => (a -> b) -> [a] -> [a]
decoSort f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

-- 2 "abcd" -> Just 'c'
-- 5 "abcd" -> Nothing
charAt :: Int -> String -> Maybe Char
charAt x = fmap fst . uncons . drop x

-- [1, 2] -> Just (1, 2)
-- [1, 2, 3] -> Nothing
l2p :: [a] -> Maybe (a, a)
l2p [a, b] = Just (a, b)
l2p _ = Nothing

-- [1, 2, 3] -> Just (1, 2, 3)
-- [1, 2] -> Nothing
l2p3 :: [a] -> Maybe (a, a, a)
l2p3 [a, b, c] = Just (a, b, c)
l2p3 _ = Nothing

-- (1, 2, 3, 4, 5) -> [1, 2, 3, 4, 5]
t2l :: Each s s a a => s -> [a]
t2l = toListOf each

tupleMin, tupleMax :: (Ord a, Each s s a a) => s -> a
tupleMin = minimum . t2l
tupleMax = maximum . t2l

tupleSum, tupleProduct :: (Num a, Each s s a a) => s -> a
tupleSum = sum . t2l
tupleProduct = product . t2l

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x : xs) = map (x :) (pick (k - 1) xs) <> pick k xs

slicesOf :: Int -> [a] -> [[a]]
slicesOf n = unfoldr $ \xs ->
  let (s, t) = (take n xs, drop 1 xs)
   in if length s >= n then Just (s, t) else Nothing

lookups :: (Ord k) => Map k v -> [k] -> [v]
lookups m = mapMaybe (m !?)

setLookups :: (Ord v) => Set v -> [v] -> [v]
setLookups s = mapMaybe $
  \v -> if Set.member v s then Just v else Nothing

rotate :: [a] -> [a]
rotate l = drop 1 l <> take 1 l

substring :: Int -> Int -> String -> String
substring start end = take (end - start) . drop start

binToDec :: [Bool] -> Integer
binToDec = foldl' (\acc x -> 2 * acc + toInteger (fromEnum x)) 0

decToBin :: Integer -> [Bool]
decToBin 0 = [False]
decToBin n =
  let go :: Integer -> [Integer] -> [Integer]
      go 0 r = r
      go k rs = go (k `div` 2) (k `mod` 2 : rs)
   in map (== 1) (go n [])

sqrtInt :: Int -> Int
sqrtInt = floor @Double . sqrt . fromIntegral

choose :: (Traversable t, Alternative m) => t a -> m a
choose = asum . fmap pure

-- with fair disjunction but much slower than choose
ichoose :: (Functor t, Foldable t, MonadLogic m) => t a -> m a
ichoose = foldr interleave empty . fmap pure

unions :: (Eq a) => [[a]] -> [a]
unions = foldr union []

intersections :: (Eq a) => [[a]] -> [a]
intersections xs = foldr intersect (unions xs) xs
