module Main where

import qualified Data.Map as M
import qualified Data.Array as A

type Position = (Int, Int)
type Grid = A.Array Position Char
type Segment = ((Position, Position), Int)
type Path = ([Position], Int)


-- Parsing input file --

makeGrid text = A.listArray ((1, 1), (length ls, length $ head ls)) $ concat ls
  where
    ls = lines text


-- Task 2 --

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
neighbours grid pos = filter ((/= '#') . (grid A.!)) $ neighPos pos
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

-- Find all first possible steps from selected positions (crossing positions
-- and starting position)
possibleStarts :: Grid -> [[Position]]
possibleStarts grid = if startPos `elem` crosPos
  then concatMap succs crosPos
  else concatMap succs (startPos : crosPos)
  where
    (startPos, _) = startAndGoal grid
    crosPos = findCrossingPos grid
    succs x = map (: x : []) $ neighbours grid x

-- Continue the given path (given as the first step) until reach one
-- of possible goal positions
segment :: Grid -> [Position] -> [Position] -> Segment
segment grid goals path@(x : y : rest)
  | x `elem` goals = ((last (y : rest), x), length (y : rest))
  | otherwise = segment grid goals (nextPos : path)
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

-- Find all possible successors of the given position
successors :: [Segment] -> Position -> [Position]
successors graph node = map (snd . fst) $ filter ((node ==) . fst . fst) graph

-- Depth-first search
search :: [Segment] -> Position -> [Path] -> Int -> Int
search _ _ [] maxDl = maxDl
search graph goal paths@(((x : xs), dl) : rest) maxDl
  | x == goal = search graph goal rest (max dl maxDl)
  | otherwise = case filter (`notElem` xs) $ successors graph x of
        []       -> search graph goal rest maxDl
        nextPosx -> search graph goal (newPaths nextPosx <> rest) maxDl
  where
    newPaths posx = map extendPath posx
    extendPath pos = ((pos : x : xs), dl + segLength pos)
    segLength pos = snd . head $ filter ((== (x, pos)) . fst) graph

-- Find longest path from start to goal position
findLongestPath :: Grid -> Int
findLongestPath grid = search (segments grid) goalPos [([startPos], 0)] 0
  where
    (startPos, goalPos) = startAndGoal grid

task2 filename = findLongestPath . makeGrid <$> readFile filename

-- Useful staff --

testTask2 = (\x -> if x == 154 then "OK" else "Something went wrong")
  <$> task2 "day23-example"

main = do
  x <- task2 "day23-input"
  putStrLn $ "Task 2: " ++ show x
