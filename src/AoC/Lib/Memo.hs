module AoC.Lib.Memo where

import AoC.Lib.Prelude
import Data.Hashable
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

noMemo :: ((a -> Identity b) -> a -> Identity b) -> a -> b
noMemo f = runIdentity . fix f

{-
1. Turn a recursive function f :: (a -> b) into open recursive form:
  f :: (a -> b) -> a -> b
  f rec a = ...

2. Add a generic Monad constraint:
  f :: (Monad m) => (a -> m b) -> a -> m b

3. Pass f to one of the memoisers eg. memoMap:
  memoisedF :: a -> b
  memoisedF = memoMap f
-}
type Memo k v = State (Map k v) v

memoMap :: forall k v. (Ord k) => ((k -> Memo k v) -> k -> Memo k v) -> k -> v
memoMap f = flip evalState mempty . f memo
  where
    -- Passed to f, the open recursive function we are memoising, as its "rec"
    -- If we already have a result return it otherwise compute and save it
    memo :: k -> Memo k v
    memo k = do
      m <- get
      case m !? k of
        Just v -> pure v
        Nothing -> do
          v <- f memo k
          modify (Map.insert k v)
          pure v

type IntMemo v = State (IntMap v) v

memoIntMap :: (Hashable k) => ((k -> IntMemo v) -> k -> IntMemo v) -> k -> v
memoIntMap = memoIntMapOn hash

memoIntMapOn :: forall k v. (k -> Int) -> ((k -> IntMemo v) -> k -> IntMemo v) -> k -> v
memoIntMapOn project f = flip evalState mempty . f memo
  where
    memo :: k -> IntMemo v
    memo k = do
      m <- get
      case IntMap.lookup (project k) m of
        Just v -> pure v
        Nothing -> do
          v <- f memo k
          modify (IntMap.insert (project k) v)
          pure v
