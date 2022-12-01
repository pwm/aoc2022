module AoC.Lib.Graph where

import AoC.Prelude
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

dfs :: forall n. (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [n]
dfs nexts isDest start = go Set.empty [start]
  where
    go :: Set n -> [n] -> [n]
    go _ [] = []
    go seen (x : xs)
      | isDest x = [x]
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) (nexts x <> xs)

bfs :: forall n. (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [n]
bfs nexts isDest start = go Set.empty (Seq.fromList [start])
  where
    go :: Set n -> Seq n -> [n]
    go _ Seq.Empty = []
    go seen (x :<| xs)
      | isDest x = [x]
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) (xs >< Seq.fromList (nexts x))

dijkstra :: (Ord n) => (n -> [(n, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
dijkstra f =
  let noHeur (n, c) = (n, c, 0)
   in astar (map noHeur . f)

astar :: forall n. (Ord n) => (n -> [(n, Int, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
astar nexts isDest start = go Set.empty (MinPQueue.singleton 0 start)
  where
    go :: Set n -> MinPQueue Int n -> [(n, Int)]
    go seen unseen = case MinPQueue.minViewWithKey unseen of
      Nothing -> []
      Just ((cost, node), unseen')
        | isDest node -> [(node, cost)]
        | Set.member node seen -> go seen unseen'
        | otherwise ->
            let f q (node', cost', heur) = MinPQueue.insert (cost + cost' + heur) node' q
             in (node, cost) : go (Set.insert node seen) (foldl' f unseen' (nexts node))
