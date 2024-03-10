module Main where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Cache = Map (String, [Int]) Int

-- Parsing input file --

parseData :: String -> [(String, [Int])]
parseData xs =
  zip (map (!! 0) records) (map (map read . splitOn "," . (!! 1)) records)
  where
    records = map words . lines $ xs

-- Task 1 --

-- Prepare empty cache and start search
search :: String -> [Int] -> Int
search x y = snd $ searchMemo M.empty x y

-- Depth-first tree search with cache memoization
searchMemo :: Cache -> String -> [Int] -> (Cache,Int)
searchMemo m [] []      = (m, 1)
searchMemo m [] (_ : _) = (m, 0)
searchMemo m (x : y) []
  | x `elem` ".?" = searchMemo m y []
  | otherwise     = (m, 0)
searchMemo m z@(x : y) c@(a : b)
  | length z < sum (intersperse 1 c) = (m, 0)
  | x == '.' = searchMemo m y c
  | x == '#' && b == [] =
      case all (`elem` "#?") (take (a - 1) y) of
        True -> searchMemo m (drop a z) b
        False -> (m, 0)
  | x == '#' =
      case (all (`elem` "#?") $ take (a - 1) y)
           && (head (drop a z) `elem` ".?") of
        True -> searchMemo m (drop a y) b
        False -> (m, 0)
  | x == '?' =
      case M.lookup (z, c) m of
        Just v -> (m, v)
        Nothing -> let (m1, v1) = searchMemo m y c
                       (m2, v2) = searchMemo m1 ('#' : y) c
                       m3 = M.insert (z, c) (v1 + v2) m2
                   in (m3, v1 + v2)

-- Unfold data and count the number of possible arrangements for all patterns
countAllArrangements :: Int -> [([Char], [Int])] -> Int
countAllArrangements r = sum . map (uncurry search . makeUnfolded)
  where
    makeUnfolded (x, y) = (unfoldData x, unfoldGroups y)
    unfoldGroups = concat . replicate r
    unfoldData = concat . intersperse "?" . replicate r

task1 filename = countAllArrangements 1 . parseData <$> readFile filename

-- Task 2 --

task2 filename = countAllArrangements 5 . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 21 then "OK" else "Something went wrong")
    <$> task1 "day12-example"

testTask2 =
  (\x -> if x == 525152 then "OK" else "Something went wrong")
    <$> task2 "day12-example"

main :: IO ()
main = do
  x <- task1 "day12-input"
  y <- task2 "day12-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
