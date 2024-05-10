module Main where


import Data.Ord
import Data.Tuple (swap)
import Data.List (sort, sortOn, sortBy, group, groupBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import System.Random (mkStdGen, randoms, randomIO)

type Node = String
type Edge = (Node, Node)
type Graph = M.Map Node [Node]

-- Parsing input file --

parseData :: String -> Graph
parseData =
  M.fromList . groupEdges . concatMap (makeEdges . splitOn ": ") . lines
  where
    makeEdges [x, ys] = concatMap (\y -> [(x, y), (y, x)]) $ words ys

groupEdges :: [Edge] -> [(String, [String])]
groupEdges = map collect . groupBy (\(x, _) (y, _) -> x == y) . sort
  where
    collect xs = (fst $ head xs, map snd xs)


-- Task 1 --

-- Select n node pairs randomly
randomPairs :: Graph -> Int -> Int -> [(Node, Node)]
randomPairs graph seed n =
  take n . map snd . sort . zip rands $ makePairs (M.keys graph)
  where
    makePairs nodes = [(x, y) | x <- nodes, y <- nodes, x < y]
    rands = randoms (mkStdGen seed) :: [Int]

-- Searching path in graph
search :: Graph -> (Node, Node) -> [Edge]
search graph (node1, node2) = map (\x -> min x $ swap x) $ zip path (tail path)
  where
    path = bfs graph node1 [[node2]] [node1]

-- Breadth first search
bfs :: Graph -> Node -> [[Node]] -> [Node] -> [Node]
bfs graph goal [] closed = []
bfs graph goal open@((x : rest) : openRest) closed
  | x == goal = head open
  | x `elem` closed = bfs graph goal openRest closed
  | otherwise = if null posNext
                then bfs graph goal openRest closed
                else bfs graph goal
                     (openRest <> map (: (x : rest)) posNext) (x : closed)
  where
    posNext = filter (`notElem` rest) $ graph M.! x

-- Count frequencies of edges in paths between node pairs
edgeFrequencies :: Graph -> [(Node, Node)] -> [(Int, Edge)]
edgeFrequencies graph =
  map count . group . sort . concatMap (search graph)
  where
    count xs = (length xs, head xs)

-- Remove an edge from graph
updateGraph :: Graph -> Edge -> Graph
updateGraph graph (n1, n2) = M.insert n2 newN2 $ M.insert n1 newN1 graph
  where
    newN1 = filter (/= n2) $ graph M.! n1
    newN2 = filter (/= n1) $ graph M.! n2

--  Count the number of nodes in the subgraph containing given nodes
floodGraph :: Graph -> [Node] -> Int
floodGraph graph nodes = if null neighbours
  then length . map head . group $ sort nodes
  else floodGraph graph $ nodes <> neighbours
  where
    neighbours = filter (`notElem` nodes) $ concatMap (graph M.!) nodes

-- Cut graph into two subgraphs by removing 3 edges
cutGraph :: Graph -> [(Node, Node)] -> (Int, Int)
cutGraph graph pairs = (subgraphSize, M.size graph - subgraphSize)
  where
    edges = take 3 . sortOn Down $ edgeFrequencies graph pairs
--    edges = take 3 . reverse . sort $ edgeFrequencies graph pairs
    newGraph = foldl (\g e -> updateGraph g $ snd e) graph edges
    subgraphSize = floodGraph newGraph [fst . snd . head $ edges]

main = do
  seed  <- randomIO :: IO Int
  graph <- parseData <$> readFile "day25-input"
  let randPairs  = randomPairs graph seed 100
  let partitions = cutGraph graph randPairs
  putStrLn $ show partitions
