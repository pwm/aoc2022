module AoC.Lib.Memo where

import AoC.Prelude
import Control.Monad.State.Strict
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

noMemo :: ((a -> Identity b) -> a -> Identity b) -> a -> b
noMemo f = runIdentity . fix f

type Memo k v = State (Map k v) v

memoMap :: forall k v. (Ord k) => ((k -> Memo k v) -> k -> Memo k v) -> k -> v
memoMap f k' = evalState (f go k') mempty
  where
    go :: k -> Memo k v
    go k = do
      m <- get
      case m !? k of
        Just v -> pure v
        _ -> do
          v <- f go k
          modify (Map.insert k v)
          pure v

type IntMemo v = State (IntMap v) v

memoIntMap :: (Hashable k) => ((k -> IntMemo v) -> k -> IntMemo v) -> k -> v
memoIntMap = memoIntMapOn hash

memoIntMapOn :: forall k v. (k -> Int) -> ((k -> IntMemo v) -> k -> IntMemo v) -> k -> v
memoIntMapOn rep f k' = evalState (f go k') mempty
  where
    go :: k -> IntMemo v
    go k = do
      m <- get
      case IntMap.lookup (rep k) m of
        Just v -> pure v
        _ -> do
          v <- f go k
          modify (IntMap.insert (rep k) v)
          pure v
