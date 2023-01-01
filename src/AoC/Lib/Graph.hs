module AoC.Lib.Graph where

import AoC.Lib.Prelude
import Data.Map.Strict qualified as Map
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

dfs :: forall n. (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [n]
dfs nexts found root = go mempty [root]
  where
    go :: Set n -> [n] -> [n]
    go _ [] = []
    go seen (node : unseen)
      | found node = [node]
      | Set.member node seen = go seen unseen
      | otherwise = node : go (Set.insert node seen) (nexts node <> unseen)

-- Explores the whole graph
dfsAll :: (Ord n) => (n -> [n]) -> n -> [n]
dfsAll nexts = dfs nexts (const False)

-- All paths from root to a dest node
dfsPaths :: forall n. (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [[n]]
dfsPaths nexts found = map reverse . go [] mempty
  where
    go :: [n] -> Set n -> n -> [[n]]
    go path seen node
      | found node = [node : path]
      | Set.member node seen = []
      | otherwise = nexts node >>= go (node : path) (Set.insert node seen)

-- Literally selects the shortest path from all paths to dest
dfsSP :: (Ord n) => (n -> [n]) -> n -> n -> [n]
dfsSP nexts dest = minimumBy (comparing length) . dfsPaths nexts (== dest)

bfs :: forall n. (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [n]
bfs nexts found root = go mempty (Seq.fromList [root])
  where
    go :: Set n -> Seq n -> [n]
    go _ Seq.Empty = []
    go seen (node :<| unseen)
      | found node = [node]
      | Set.member node seen = go seen unseen
      | otherwise = node : go (Set.insert node seen) (unseen <> Seq.fromList (nexts node))

bfsAll :: (Ord n) => (n -> [n]) -> n -> [n]
bfsAll nexts = bfs nexts (const False)

-- Keeps track of parents. BFS as a special case of Dijkstra's with weights of 1
bfsWithParents :: (Ord n) => (n -> [n]) -> (n -> Bool) -> n -> [(n, Maybe n)]
bfsWithParents nexts found =
  map (second snd)
    . dijkstraWithParents (map (,1) . nexts) found

-- Shortest path from root to dest
bfsSP :: (Ord n) => (n -> [n]) -> n -> n -> [n]
bfsSP nexts dest =
  map fst
    . buildSP dest
    . dijkstraWithParents (map (,1) . nexts) (== dest)

-- Shortest-path tree (aka. BFS tree which, in unweighted graphs, coincide with the SPT)
bfsSPT :: (Ord n) => (n -> [n]) -> n -> Tree n
bfsSPT nexts =
  fmap fst
    . buildSPT
    . dijkstraWithParents (map (,1) . nexts) (const False)

dijkstra :: (Ord n) => (n -> [(n, Int)]) -> (n -> Bool) -> n -> [(n, Int)]
dijkstra nexts found = map (second fst) . dijkstraWithParents nexts found

dijkstraAll :: (Ord n) => (n -> [(n, Int)]) -> n -> [(n, Int)]
dijkstraAll nexts = dijkstra nexts (const False)

-- Dijkstra's as a special case of A* using 0 for heuristic
dijkstraWithParents :: (Ord n) => (n -> [(n, Int)]) -> (n -> Bool) -> n -> [(n, (Int, Maybe n))]
dijkstraWithParents nexts = astarWithParents (map (second (,0)) . nexts)

-- Shortest path with dists from root to dest
dijkstraSP :: (Ord n) => (n -> [(n, Int)]) -> n -> n -> [(n, Int)]
dijkstraSP nexts dest = buildSP dest . dijkstraWithParents nexts (== dest)

-- Shortest-path tree with dists
dijkstraSPT :: (Ord n) => (n -> [(n, Int)]) -> n -> Tree (n, Int)
dijkstraSPT nexts = buildSPT . dijkstraWithParents nexts (const False)

astar :: (Ord n) => (n -> [(n, (Int, Int))]) -> (n -> Bool) -> n -> [(n, Int)]
astar nexts found = map (second fst) . astarWithParents nexts found

astarAll :: (Ord n) => (n -> [(n, (Int, Int))]) -> n -> [(n, Int)]
astarAll nexts = astar nexts (const False)

-- The most generic algo here, uses heuristic and keeps track of parents
astarWithParents :: forall n. (Ord n) => (n -> [(n, (Int, Int))]) -> (n -> Bool) -> n -> [(n, (Int, Maybe n))]
astarWithParents nexts found root = go mempty (MinPQueue.singleton 0 (root, Nothing))
  where
    go :: Set n -> MinPQueue Int (n, Maybe n) -> [(n, (Int, Maybe n))]
    go seen unseen = case MinPQueue.minViewWithKey unseen of
      Nothing -> []
      Just ((cost, (node, parent)), unseen')
        | found node -> [(node, (cost, parent))]
        | Set.member node seen -> go seen unseen'
        | otherwise ->
            let addNode :: MinPQueue Int (n, Maybe n) -> (n, (Int, Int)) -> MinPQueue Int (n, Maybe n)
                addNode mpq (n, (stepCost, h)) = MinPQueue.insert (cost + stepCost + h) (n, Just node) mpq
             in (node, (cost, parent)) : go (Set.insert node seen) (foldl' addNode unseen' (nexts node))

astarSP :: (Ord n) => (n -> [(n, (Int, Int))]) -> n -> n -> [(n, Int)]
astarSP nexts dest = buildSP dest . astarWithParents nexts (== dest)

astarSPT :: (Ord n) => (n -> [(n, (Int, Int))]) -> n -> Tree (n, Int)
astarSPT nexts = buildSPT . astarWithParents nexts (const False)

-- Builds shortest path from root (identified by Nothing as its parent) to dest
buildSP :: forall n. (Ord n) => n -> [(n, (Int, Maybe n))] -> [(n, Int)]
buildSP dest (Map.fromList -> m) = reverse $ go [] dest
  where
    go :: [(n, Int)] -> n -> [(n, Int)]
    go path node = case m !? node of
      Nothing -> []
      Just (cost, mp) -> case mp of
        Nothing -> (node, cost) : path
        Just parent -> (node, cost) : go path parent

-- Builds shortest path tree from root (identified by Nothing as its parent)
buildSPT :: forall n. (Ord n) => [(n, (Int, Maybe n))] -> Tree (n, Int)
buildSPT (Map.fromList -> m) =
  let (root, adjList) = Map.foldrWithKey mkAdjList (error "Root not found", mempty) m
   in unfoldTree (\(n, d) -> ((n, d), fromMaybe [] (adjList !? n))) (root, 0)
  where
    mkAdjList :: n -> (Int, Maybe n) -> (n, Map n [(n, Int)]) -> (n, Map n [(n, Int)])
    mkAdjList n (_, Nothing) (_, adjList) = (n, adjList) -- root
    mkAdjList n (d, Just p) (r, adjList) = (r, Map.insertWith (<>) p [(n, d)] adjList)
