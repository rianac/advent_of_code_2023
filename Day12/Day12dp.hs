module Main where

import Data.List (tails, intersperse, intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype Key = Key (String, [Int]) deriving (Show, Eq)

instance Ord Key where
   compare (Key (x, y)) (Key (a, b))
    | length x < length a = LT
    | length x > length a = GT
    | length y < length b = LT
    | length y > length b = GT
    | otherwise           = EQ

type Table = Map Key Int

-- Parsing input file --

parseData :: String -> [(String, [Int])]
parseData xs =
  zip (map (!! 0) records) (map (map read . splitOn "," . (!! 1)) records)
  where
    records = map words . lines $ xs

-- Task 1 --

-- Prepare table for all pairs "state-value" with initialization to
-- default (0) or informed values (terminal states with known values)
makeTable :: String -> [Int] -> Table
makeTable springs signatures =
  M.fromList terminalStates `M.union` M.fromList inits
  where
    inits = [ (Key (x, y), 0) | x <- tails springs, y <- tails signatures]
    findTerminalStates = tails . reverse . takeWhile (`elem` ".?") . reverse
    terminalStates = [ (Key (x, []), 1) | x <-  findTerminalStates springs]

-- Dynamic programming using sweep update strategy
dynProgSweep :: String -> [Int] -> Int
dynProgSweep springs signatures = finalTable M.! Key (springs, signatures)
  where
    iterations = iterate
                   (\(_, t) -> (t, M.mapWithKey (updateRecord t) t))
                   (M.empty, makeTable springs signatures)
    finalTable = fst . head $ dropWhile (\(x, y) -> x /= y) iterations

-- Dynamic programming using in-place update strategy
dynProgInPlace :: String -> [Int] -> Int
dynProgInPlace springs signatures = finalTable M.! Key (springs, signatures)
  where
    iterations = iterate
                   (\(_, t) -> (t, mapTable t))
                   (M.empty, makeTable springs signatures)
    finalTable = fst . head $ dropWhile (\(x, y) -> x /= y) iterations
    mapTable table = foldl
                       (\t k -> M.insert k (updateRecord t k (t M.! k)) t)
                       table
                       (M.keys table)

-- Rules for updating state values
updateRecord :: Table -> Key -> Int -> Int
updateRecord t (Key ((x : y), (a : b))) _
      | length (x : y) < sum (intersperse 1 (a : b)) = 0
      | x == '.' = t M.! Key (y, (a : b))
      | x == '#' = let dropLength = if null b then a - 1 else a
                   in if all (`elem` "#?") (take (a - 1) y)
                         && (null b || y !! (a - 1) `elem` ".?")
                      then t M.! Key (drop dropLength y, b) else 0
      | x == '?' = updateRecord t (Key (('.' : y), (a : b))) 0
                   + updateRecord t (Key (('#' : y), (a : b))) 0
updateRecord _ _ v = v

-- Unfold data and count the number of possible arrangements for all patterns
countArrangements :: (String -> [Int] -> Int) -> Int -> [(String, [Int])] -> Int
countArrangements strategy r = sum . map (uncurry strategy . makeUnfolded)
  where
    makeUnfolded (x, y) = (unfoldData x, unfoldGroups y)
    unfoldGroups = concat . replicate r
    unfoldData = intercalate "?" . replicate r

task1 s filename = countArrangements s 1 . parseData <$> readFile filename

-- Task 2 --

task2 s filename = countArrangements s 5 . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x y -> if (x, y) == (21, 21) then "OK" else "Something went wrong")
    <$> task1 dynProgSweep "day12-example"
    <*> task1 dynProgInPlace "day12-example"

testTask2 =
  (\x y -> if (x, y) == (525152, 525152) then "OK" else "Something went wrong")
    <$> task2 dynProgSweep "day12-example"
    <*> task2 dynProgInPlace "day12-example"

main :: IO ()
main = do
  x <- task1 dynProgInPlace "day12-input"
  y <- task2 dynProgInPlace "day12-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
