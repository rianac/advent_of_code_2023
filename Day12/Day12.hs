module Main where

import Data.List (intersperse, intercalate)
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP

-- Parsing input file --

parseData :: String -> [(String,[Int])]
parseData xs =
  zip (map (!! 0) records) (map (map read .  splitOn "," . (!! 1)) records)
  where
    records =  map  words . lines $ xs

-- Task 1 --

damagedP :: Int -> ReadP String
damagedP x = count x $ satisfy (`elem` "?#")

ok1P, okP :: ReadP String
ok1P       = many1 $ satisfy (`elem` ".?")
okP        = many $ satisfy (`elem` ".?")

-- Prepare a list of required parsers
makeParsersP :: [Int] -> [ReadP String]
makeParsersP xs = intersperse ok1P (map damagedP xs)

-- Combine parsers into a sequence
composeParsersP :: [ReadP String] -> ReadP [String]
composeParsersP [a,b,c] =
  let fun _ _ _ = []
  in fun <$> a <*> b <*> c
composeParsersP [a,b,c,d,e] =
  let fun _ _ _ _ _ = []
  in fun <$> a <*> b <*> c <*> d <*> e
composeParsersP [a,b,c,d,e,f,g] =
  let fun _ _ _ _ _ _ _ = []
  in fun <$> a <*> b <*> c <*> d <*> e <*> f <*> g
composeParsersP [a,b,c,d,e,f,g,h,i] =
  let fun _ _ _ _ _ _ _ _ _ = []
  in fun <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i
composeParsersP [a,b,c,d,e,f,g,h,i,j,k] =
  let fun _ _ _ _ _ _ _ _ _ _ _ = []
  in fun <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k
composeParsersP _ = error "Unpredicted number of contiguous groups"

-- Count the number of possible arrangements for all patterns
countAllArrangements :: [(String, [Int])] -> Int
countAllArrangements = sum . map (length . match)
  where
    match (x, y) = readP_to_S (finalizeParser $ makeParser y) x
    makeParser = composeParsersP . makeParsersP
    finalizeParser p = okP *> p <* okP <* eof

task1 filename =  countAllArrangements . parseData <$> readFile filename

-- Task 2 --

-- Count the number of possible arrangements for one pattern
countArrangements :: (String, [Int]) -> Int
countArrangements (x, y) =
  length $ readP_to_S (prepareParser y) x
  where
    makeParser = foldl1 (*>) . intersperse ok1P . map damagedP
    finalizeParser p = okP *> p <* okP <* eof
    prepareParser = finalizeParser . makeParser

-- Unfold data and count the number of possible arrangements for all patterns
countAllArrangements2 :: Int -> [(String, [Int])] -> Int
countAllArrangements2 r = sum . map (countArrangements . makeUnfolded)
  where
    makeUnfolded (x, y) = (unfoldData x, unfoldGroups y)
    unfoldGroups = concat . replicate r
    unfoldData = intercalate "?" . replicate r

task1alt filename =  countAllArrangements2 1 . parseData <$> readFile filename

task2 filename =  countAllArrangements2 5 . parseData <$> readFile filename

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
  x2 <- task1 "day12-input"
  y <- task2 "day12-input"
  putStrLn $ "Task 1: " ++ show x ++ " or " ++ show x2
  putStrLn $ "Task 2: " ++ show y
