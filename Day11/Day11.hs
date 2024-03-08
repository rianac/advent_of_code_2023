module Main where

import Data.List ((\\))

type Galaxy = (Int, Int)

-- Parse input file --

parseData :: String -> [Galaxy]
parseData = findGalaxies . lines
  where
    findGalaxies xs = [ (i, j)
                      | (i, r) <- zip [1..] xs
                      , (j, e) <- zip [1..] r
                      , e == '#']

-- Task 1 --

-- Find empty rows/columns which are without any galaxy 
findEmpty :: [Galaxy] -> ([Int], [Int])
findEmpty xs = (empty $ map fst xs, empty $ map snd xs)
  where
    empty xs = dropWhile (< minimum xs) . takeWhile (< maximum xs) $ [1..] \\ xs

-- Calculate distance between two galaxies including expansion of emty space
distance :: (Int, Int) -> ([Int], [Int]) -> Galaxy -> Galaxy -> Int
distance (rowExpansion, colExpansion) (emptyRows, emptyCols) (a,b) (c,d) =
  let expandedRows = filter (`elem` emptyRows) [min a c .. max a c]
      expandedCols = filter (`elem` emptyCols) [min b d .. max b d]
  in  abs (a - c) + abs (b - d)
      + (rowExpansion - 1) * length expandedRows
      + (colExpansion - 1) * length expandedCols

-- Find sum of distances between each pair of galaxies
findDistances :: (Int, Int) -> [Galaxy] -> Int
findDistances expansions xs =
  sum [distance expansions empty x y | x <- xs, y <- xs, x < y]
  where empty = findEmpty xs

task1 filename = findDistances (2, 2) . parseData <$> readFile filename

-- Task 2 --

task2 ew filename = findDistances ew . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 374 then "OK" else "Something went wrong")
    <$> task1 "day11-example"

testTask2 =
  (\x y -> if (x, y) == (1030, 8410) then "OK" else "Something went wrong")
    <$> task2 (10,10) "day11-example"
    <*> task2 (100,100) "day11-example"

main :: IO ()
main = do
  x <- task1 "day11-input"
  y <- task2 (1000000, 1000000) "day11-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
