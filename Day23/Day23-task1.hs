module Main where

import qualified Data.Map as M
import qualified Data.Array as A

type Position = (Int, Int)
type Grid = A.Array Position Char
type Segment = ((Position, Position), Int)

-- Parsing input file --

makeGrid text = A.listArray ((1, 1), (length ls, length $ head ls)) $ concat ls
  where
    ls = lines text


-- Task 1 --

-- Find all crossing positions - positions with more than 2 valid neighbours
findCrossingPos :: Grid -> [Position]
findCrossingPos grid =
  [pos | pos <- innerPos, length (neighbours grid pos) > 2]
  where
    ((rmin, cmin), (rmax, cmax)) = A.bounds grid
    innerPos = [(r, c) | r <- [rmin .. rmax], c <- [cmin .. cmax]
                       , grid A.! (r, c) /= '#']

-- Find all valid neighbours of the given position
neighbours :: Grid -> Position -> [Position]
neighbours grid = filter ((/= '#') . (grid A.!)) . neighPos
  where
    ((rmin, cmin), (rmax, cmax)) = A.bounds grid
    neighPos (r, c) =
      filter (\(x, y) -> x >= rmin && x <= rmax && y >= cmin && y <= cmax)
      [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- Find start and goal positions in the first and last rows
startAndGoal :: Grid -> (Position, Position)
startAndGoal grid = (startPos, goalPos)
  where
    ((rmin, cmin), (rmax, cmax)) = A.bounds grid
    startPos = head . filter ((== '.') . (grid A.!)) $ (1,) <$> [1 .. cmax]
    goalPos  = head . filter ((== '.') . (grid A.!)) $ (rmax,) <$> [1 .. cmax]

-- Test whether step from the first to the second position is valid
validStep :: Grid -> Position -> Position -> Bool
validStep grid (a,b) t@(c,d)
      | a + 1 == c = grid A.! t `elem` ".v"
      | a - 1 == c = grid A.! t `elem` "."
      | b + 1 == d = grid A.! t `elem` ".>"
      | b - 1 == d = grid A.! t `elem` "."
      | otherwise = error "unexpected step"

-- Find all first possible steps from selected positions (crossing positions
-- and starting position)
possibleStarts :: Grid -> [[Position]]
possibleStarts grid = if startPos `elem` crosPos
  then concatMap succs crosPos
  else concatMap succs (startPos : crosPos)
  where
    (startPos, _) = startAndGoal grid
    crosPos  = findCrossingPos grid
    succs x = map (: x : []) $ filter (validStep grid x) $ neighbours grid x

-- Continue the given path (given as the first step) until reaches one
-- of possible target positions
segment :: Grid -> [Position] -> [Position] -> Segment
segment grid targets path@(x : y : rest)
  | x `elem` targets = ((last (y : rest), x), length (y : rest))
  | otherwise = if validStep grid x nextPos
                then segment grid targets (nextPos : path)
                else error "got stuck"
  where
    nextPos = head $ filter (/= y) $ neighbours grid x

-- Find length of all possile paths between selected positions (crossing
-- positions and starting position)
segments :: Grid -> [Segment]
segments grid =
  map (segment grid (startPos : goalPos : crosPos)) $ possibleStarts grid
  where
    ((rmin, cmin), (rmax, cmax)) = A.bounds grid
    (startPos, goalPos) = startAndGoal grid
    crosPos = findCrossingPos grid

-- Find starting position of all segments ending in the given node
parents :: [Segment] -> Position -> [Position]
parents dag node =  map (fst . fst) $ filter ((node ==) . snd . fst) dag

-- Order nodes of DAG into a linear sequence
linearizeDAG :: [Segment] -> [Position] -> [Position] -> [Position]
linearizeDAG _ ordNodes [] = ordNodes
linearizeDAG dag ordNodes nodes =
  linearizeDAG dag (selected <> ordNodes) (filter (`notElem` selected) nodes)
  where
    selected = filter (all (`elem` ordNodes) . parents dag) nodes

-- Find length of longest path to all dag nodes (crossing positions + goal)
longestPaths :: [Segment] -> [Position] -> M.Map Position Int
longestPaths dag ordNodes =
  foldr updateLengths (M.singleton (last ordNodes) 0) ordNodes
  where
    updateLengths pos lengths = M.insert pos (pathLength pos lengths) lengths
    pathLength pos lengths = maximum $
      0 : map (\x -> (snd . head $ filter ((== (x, pos)) . fst) dag)
                  + (lengths M.! x)) (parents dag pos)

-- Find longest path from start to goal position
findLongestPath :: Grid -> Int
findLongestPath grid = longestPaths dag linearDAG M.! head linearDAG
  where
    crosPos = findCrossingPos grid
    (startPos, goalPos) = startAndGoal grid
    dag = segments grid
    linearDAG = linearizeDAG dag [startPos] (goalPos : crosPos)

task1 filename = findLongestPath . makeGrid <$> readFile filename

-- Useful staff --

testTask1 = (\x -> if x == 94 then "OK" else "Something went wrong")
  <$> task1 "day23-example"

main = do
  x <- task1 "day23-input"
  putStrLn $ "Task 1: " ++ show x
