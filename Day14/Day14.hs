module Main where

import Data.List (transpose, intercalate, sort)
import Data.List.Split (splitOn)

type Grid = [[Place]]
data Place = Oval | Free | Cube deriving (Show, Eq, Ord)

-- Parse input file --

parseData = transpose . map (map codeItem) . lines
  where
    codeItem 'O' = Oval
    codeItem '.' = Free
    codeItem '#' = Cube

-- Task 1 --

-- Roll oval stones towards beginning of lines
tilt :: Grid -> Grid
tilt = map $ intercalate [Cube] . map sort . splitOn [Cube]

-- Calculate load based on distance of oval stones to bottom of grid
calculateLoad :: Grid -> Int
calculateLoad = sum . map loadOfLine
  where
    loadOfLine = sum . zipWith (\x y -> x * weight y) [1 ..] . reverse
    weight x = if x == Oval then 1 else 0

task1 filename = calculateLoad . tilt . parseData
  <$> readFile filename

-- Task 2 --

-- Rotate grid by 90° anticlockwise
rotate90 :: Grid -> Grid
rotate90 = transpose . map reverse

-- Roll oval stones subsequently in all directions
tiltCycle :: Grid -> Grid
tiltCycle grid = foldr tiltAndRotate grid ["N", "W", "S", "E"]
  where
    tiltAndRotate _ = rotate90 . tilt

-- Try to find a periodic repetition in the serie and its beginning
searchRepetition :: Grid -> ([Grid], Int)
searchRepetition grid = go grid []
  where
    go grid history =
      if grid `elem` history
      then (reverse $ grid : takeWhile (/= grid) history,
            length $ dropWhile (/= grid) history)
      else go (tiltCycle grid) (grid : history)

-- Cheat a bit - do not perform all spinnings but calculate which
-- spinning will be the final one
solve :: Grid -> Int
solve grid = calculateLoad $ repeatedHistory !! pos
  where
    (repeatedHistory, rampPhaseLength) = searchRepetition grid
    pos = (1000000000 - rampPhaseLength) `mod` length repeatedHistory

task2 filename = solve . parseData  <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 136 then "OK" else "Something went wrong")
    <$> task1 "day14-example"

testTask2 =
  (\x -> if x == 64 then "OK" else "Something went wrong")
    <$> task2 "day14-example"

main :: IO ()
main = do
  x <- task1 "day14-input"
  y <- task2 "day14-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
