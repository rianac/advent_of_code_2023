module Main where

import Data.List (tails, inits)
import Data.List.Split (splitOn)
import Data.Matrix (fromLists, toLists, transpose)
import Data.Tuple.Extra ((&&&), first, second)

-- Parsing input file --

parseData = map (\x -> (x, toLists $ transpose $ fromLists x)) . grids
  where
    grids = map (splitOn "\n") . splitOn "\n\n" . init

-- Task 1 --

-- Produce reflection pairs for different mirroring lines
mirrorPatterns :: Int -> [(Int, [(Int, Int)])]
mirrorPatterns x = zip [1..] . tail . init $ patterns
  where
    patterns = uncurry (zipWith zip) . first (map reverse) $ chunks
    chunks = (inits &&& tails) [0 .. x - 1]

-- Check vertical mirroring of grid (given as a list of rows)
checkMirror :: [String] -> Int
checkMirror grid = if null checks then 0 else fst $ head checks
  where
    patterns = mirrorPatterns $ length grid
    checks = filter snd $ map checkPattern patterns
    checkPattern = second $ all (\(x, y) -> grid !! x == grid !! y)

-- Count mirrorings of grids (vertical reflections) and transposed grids
-- (horizontal reflections)
countReflections :: ([String] -> Int) -> [([String], [String])] -> Int
countReflections f = sum . map checkGrid
  where
    checkGrid (gr, gc) = 100 * f gr + f gc

task1 filename = countReflections checkMirror . parseData <$> readFile filename

-- Task 2 --

-- Check vertical mirroring of grid (given as a list of rows) possible
-- by changing one character in the grid
checkPossibleMirror :: [String] -> Int
checkPossibleMirror grid = if null checks then 0 else fst . head $ checks
  where
    patterns = mirrorPatterns $ length grid
    checks = filter ((== 1) . snd) $ map checkPattern patterns
    checkPattern =
      second (sum . map (\(x, y) -> stringDiff (grid !! x) (grid !! y)))

-- Count different value pairs
stringDiff :: String -> String -> Int
stringDiff x y = sum $ zipWith (\x y -> if x == y then 0 else 1) x y

task2 filename =
  countReflections checkPossibleMirror . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 405 then "OK" else "Something went wrong")
    <$> task1 "day13-example"

testTask2 =
  (\x -> if x == 400 then "OK" else "Something went wrong")
    <$> task2 "day13-example"

main :: IO ()
main = do
  x <- task1 "day13-input"
  y <- task2 "day13-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
