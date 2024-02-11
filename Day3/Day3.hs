module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

{- Task 1:
To sum all the numbers which are adjacent to a symbol - any number adjacent
to a symbol, even diagonally, is a "part number" and should be included in
your sum.
-}

type Position = (Int,Int)
data Part = Number Int | Symbol Char  deriving (Eq, Show)

-- Parse one line

numberP :: ReadP String
numberP = munch1 isDigit

voidP :: ReadP String
voidP = munch1 (=='.')

symbolP :: ReadP String
symbolP = fmap (:[]) get

tokensP :: ReadP [String]
tokensP = many1 $ numberP <++ voidP <++ symbolP

-- Parse one line and instantiate respective data type
parseLine :: (Int,String) -> [(Part,[Position])]
parseLine (lineId, line) =
  let parsed_line = fst . head $ readP_to_S (tokensP <* eof) line
  in makeParts lineId parsed_line 1
  where
    makeParts _ [] _ = []
    makeParts id (x@(y:_):xs) start
      | y == '.' = makeParts id xs (start + length x)
      | isDigit y =
          let end = start + length x - 1
              part = (Number $ read x, map (id,) [start..end])
          in part : makeParts id xs (end + 1)
      | otherwise = (Symbol y, [(id,start)]) : makeParts id xs (start + 1)

-- Test whether two parts are adjacent (difference in position is 1)
isAdjacent :: (Part, [Position]) -> (Part, [Position]) -> Bool
isAdjacent (_, p1) (_, p2) =
  let dist = [ max (abs $ l1-l2) (abs $ c1-c2) | (l1,c1)<-p1, (l2,c2)<-p2]
  in any (==1) dist

-- Concatenate parts of all lines and divide them into number and symbol lists
makeParts :: [[(Part,[Position])]] -> ([(Part,[Position])],[(Part,[Position])])
makeParts parts =
  let x = concat parts
      isNumber (Number _,_) = True
      isNumber (Symbol _,_) = False
      numbers = filter isNumber x
      symbols = filter (not . isNumber) x
  in (numbers, symbols)

-- Find numbers adjacent to any symbol and sum them
findPartNumbers :: ([(Part,[Position])],[(Part,[Position])]) -> Int
findPartNumbers (numbers,symbols) =
  sum $ map (\(Number x,_) -> x) $ findParts numbers symbols
  where
    findParts nums syms =
      filter (\x -> any (isAdjacent x) syms) nums

task1 :: FilePath -> IO Int
task1 filename =
  do
    schema <- readFile filename
    let parts = makeParts . (map parseLine) . (zip [1..]) . lines $ schema
    return . findPartNumbers $ parts

testTask1 :: IO ()
testTask1 =
  do
    res <- task1 "day3-example"
    case res of
      4361 -> putStrLn "OK"
      _ -> putStrLn "something went wrong"

{- Task 2:
 A gear is any * symbol that is adjacent to exactly two part numbers. Its
gear ratio is the result of multiplying those two numbers together. Find
the gear ratio of every gear and add them all up.
-}

-- Find number pairs adjacent to a symbol '*' and sum their products
findPartPairs :: ([(Part,[Position])],[(Part,[Position])]) -> Int
findPartPairs (numbers,symbols) =
  let stars = filter (\(Symbol x,_) -> x == '*') symbols
      adjacent_numbers = map (\s -> filter (isAdjacent s) numbers) stars
      number_pairs = filter (\x -> length x == 2) adjacent_numbers
  in sum $ map (\((Number x,_):(Number y,_):_) -> x * y) number_pairs

task2 :: FilePath -> IO Int
task2 filename =
  do
    schema <- readFile filename
    let parts = makeParts . (map parseLine) . (zip [1..]) . lines $ schema
    return . findPartPairs $ parts

testTask2 :: IO ()
testTask2 =
  do
    res <- task2 "day3-example"
    case res of
      467835 -> putStrLn "OK"
      _ -> putStrLn "something went wrong"

main :: IO ()
main =
  do
    x <- task1 "day3-input"
    y <- task2 "day3-input"
    putStrLn $ "Task 1: " ++ show x
    putStrLn $ "Task 2: " ++ show y
