module AoC.Lib.Graph where

import AoC.Lib.Prelude
import Control.Monad.State.Strict
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

dfs :: (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [n]
dfs = dfsOn id

dfsOn :: forall n r. (Ord r) => (n -> r) -> (n -> [n]) -> (n -> Bool) -> n -> [n]
dfsOn project nexts isDest start = go Set.empty [start]
  where
    go :: Set r -> [n] -> [n]
    go _ [] = []
    go seen (node : nodes)
      | isDest node = [node]
      | Set.member (project node) seen = go seen nodes
      | otherwise = node : go (Set.insert (project node) seen) (nexts node <> nodes)

paths :: (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [[n]]
paths = pathsOn id

-- all paths from a node to a node
pathsOn :: forall n r. (Ord r) => (n -> r) -> (n -> [n]) -> (n -> Bool) -> n -> [[n]]
pathsOn project nexts isDest start =
  map reverse $ execStateT (go Set.empty start) []
  where
    go :: Set r -> n -> StateT [n] [] n
    go seen node
      | isDest node = modify (node :) >> pure node
      | Set.member (project node) seen = empty
      | otherwise = do
          modify (node :)
          next <- choose (nexts node)
          go (Set.insert (project node) seen) next

bfs :: (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [n]
bfs = bfsOn id

bfsOn :: forall n r. (Ord r) => (n -> r) -> (n -> [n]) -> (n -> Bool) -> n -> [n]
bfsOn project nexts isDest start = go Set.empty (Seq.fromList [start])
  where
    go :: Set r -> Seq n -> [n]
    go _ Seq.Empty = []
    go seen (node :<| nodes)
      | isDest node = [node]
      | Set.member (project node) seen = go seen nodes
      | otherwise = node : go (Set.insert (project node) seen) (nodes >< Seq.fromList (nexts node))

dijkstra :: (Ord n) => (n -> [(n, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
dijkstra = dijkstraOn id

dijkstraOn :: (Ord r) => (n -> r) -> (n -> [(n, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
dijkstraOn project nexts =
  let noHeur (node, cost) = (node, cost, 0)
   in astarOn project (map noHeur . nexts)

astar :: (Ord n) => (n -> [(n, Int, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
astar = astarOn id

astarOn :: forall n r. (Ord r) => (n -> r) -> (n -> [(n, Int, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
astarOn project nexts isDest start = go Set.empty (MinPQueue.singleton 0 start)
  where
    go :: Set r -> MinPQueue Int n -> [(n, Int)]
    go seen unseen = case MinPQueue.minViewWithKey unseen of
      Nothing -> []
      Just ((cost, node), unseen')
        | isDest node -> [(node, cost)]
        | Set.member (project node) seen -> go seen unseen'
        | otherwise ->
            let f :: MinPQueue Int a -> (a, Int, Int) -> MinPQueue Int a
                f q (node', cost', heur) = MinPQueue.insert (cost + cost' + heur) node' q
             in (node, cost) : go (Set.insert (project node) seen) (foldl' f unseen' (nexts node))
