module Main where

import Data.List (sort, unfoldr)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Position = (Int, Int)

-- Parse input file --

parseData :: String -> Map Position [Position]
parseData = Map.fromList . fields . lines
  where
    fields xs = [((i, j), transitions i j e)
                | (i, row) <- zip [1..] xs
                , (j, e) <- zip [1..] row
                , e /= '.'
                ]
    transitions r c '|' = [(r - 1, c), (r + 1, c)]
    transitions r c '-' = [(r, c - 1), (r, c + 1)]
    transitions r c 'L' = [(r - 1, c), (r, c + 1)]
    transitions r c 'J' = [(r - 1, c), (r, c - 1)]
    transitions r c '7' = [(r, c - 1), (r + 1, c)]
    transitions r c 'F' = [(r, c + 1), (r + 1, c)]
    transitions r c 'S' = []
    transitions _ _ _ = error "Unexpected pipe bend type"

-- Task 1 --

-- Find starting point and select one of its successors
findStart :: Map Position [Position] -> [Position]
findStart m =
  let transitions = Map.assocs m
      start       = fst . head $ filter ((== []) . snd) transitions
      sucs        = fst . head $ filter (elem start . snd) transitions
  in  [start, sucs]

-- Fing cyclic path starting and finishing in the start point
makePath :: Map Position [Position] -> [Position] -> [Position]
makePath m = unfoldr (\[x, y] -> extend [x,y] <$> Map.lookup y m)
  where
    extend [x, y] [n1, n2] = if x == n1 then (y, [y, n2]) else (y, [y, n1])
    extend [x, y] []       = (y,[y,(0,0)])  -- (0,0) is not in map m

-- Find the point on path which is the farthest point to the starting point
farthestPoint :: Map Position [Position] -> Int
farthestPoint m =
  let path = init $ makePath m $ findStart m
  in  (1 +) . length $ takeWhile (uncurry (/=)) $ zip path (reverse path)

task1 filename = farthestPoint . parseData <$> readFile filename

-- Task 2 --

-- Rotate path in order for it to start within top row
rotatePath :: [Position] -> Int -> [Position]
rotatePath path top =
  let xs = takeWhile ((/= top) . fst) path
      ys = drop (length xs) path ++ xs
  in  tail ys ++ [head ys]

-- Find all segments in one row. If both ends of a segment have the same
-- direction (up or down), then divide the segment into two segments.
rowCut :: Int -> [Position] -> [[Int]]
rowCut row path = sort $ rowCut' row path [] 0
  where
    rowCut' _ [] []  _ = []
    rowCut' _ [] acc _ = [acc]
    rowCut' r ((x, y) : rest) acc pred
      | r == x    = rowCut' r rest (y : acc) pred
      | null acc  = rowCut' r rest [] x
      | otherwise = divideSegment pred x acc ++ rowCut' r rest [] x
    divideSegment predecessor successor segment =
      let xs = sort segment
      in  if predecessor /= successor then [xs] else [[head xs], tail xs]

-- Find a cyclic path, rotate it, and make cuts through the path in each
-- row (except top and bottom rows). Count free places between every second
-- segment pairs.
findFree :: Map Position [Position] -> Int
findFree m =
  let path    = makePath m $ findStart m
      rows    = sort . deduplicate $ map fst path
      rotated = rotatePath path $ head rows
  in  sum $ concat [map freePlaces . chunksOf 2 $ rowCut r rotated
                   | r <- (init . tail) rows]
    where
      deduplicate = Set.toList . Set.fromList
      freePlaces [l, u] = head u - last l - 1

task2 filename = findFree . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x y -> if (x, y) == (4, 8) then "OK" else "Something went wrong")
    <$> task1 "day10-example1"
    <*> task1 "day10-example2"

testTask2 =
  (\x y z -> if (x, y, z) == (4, 4, 8) then "OK" else "Something went wrong")
    <$> task2 "day10-example3"
    <*> task2 "day10-example4"
    <*> task2 "day10-example5"

main :: IO ()
main = do
  x <- task1 "day10-input"
  y <- task2 "day10-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
