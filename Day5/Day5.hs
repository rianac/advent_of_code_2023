module Main where

import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha,isDigit)

type Interval = (Int,Int)
type Conversion = (Interval,Int)
type IvMap = [Conversion]

-- Parse input file

wordP :: ReadP String
wordP = munch1 isAlpha

numberP :: ReadP Int
numberP = fmap read (munch1 isDigit)

seedsP :: ReadP [Int]
seedsP = do
  string "seeds: "
  seeds <- sepBy numberP (char ' ')
  return seeds

intervalP :: ReadP Conversion
intervalP = do
  d_start <- numberP
  char ' '
  s_start <- numberP
  char ' '
  int_range <- numberP
  return ((s_start, (s_start+int_range-1)),d_start-s_start)

intervalsP :: ReadP IvMap
intervalsP = sepBy intervalP (char '\n')

mapP :: ReadP (String,IvMap)
mapP = do
  source <- wordP
  string "-to-"
  destination <- wordP
  string " map:\n"
  intervals <- intervalsP
  return (take 4 source ++ "-to-" ++ take 4 destination,intervals)

mapsP :: ReadP [(String,IvMap)]
mapsP = sepBy mapP (string "\n\n")

dataP :: ReadP ([Int], Map.Map String IvMap)
dataP = do
  seeds <- seedsP
  string "\n\n"
  maps <- mapsP
  char '\n'
  eof
  return (seeds,Map.fromList maps)

parseData :: String -> ([Int], Map.Map String IvMap)
parseData =
  fst . head . readP_to_S dataP

{- Task 1:
To convert a set of numbers (seeds) through a pipe of mappings (seeds->soil,
soil-> fertilizer, ..., humidity->location) and identify the seed corresponding
to the closest location.
-}

{-
-- Find appropriate mapping for a seed
convertNumber :: Int -> Maybe IvMap -> Int
convertNumber _ Nothing = error "Unpredicted situation"
convertNumber x (Just map) =
  case filter (containing x) map  of
       [] -> x
       ((_,y):_) -> x + y
  where
    containing x ((lower,upper),_) = x >= lower && x <= upper


-- Push a seed through a pipe of mappings to find seed's location
fullConversion :: Map.Map String IvMap -> Int -> Int
fullConversion maps seed =
  let soil = convertNumber seed $ Map.lookup "seed-to-soil" maps
      fertilizer = convertNumber soil $ Map.lookup "soil-to-fert" maps
      water = convertNumber fertilizer $ Map.lookup "fert-to-wate" maps
      light = convertNumber water $ Map.lookup "wate-to-ligh" maps
      temperature = convertNumber light $ Map.lookup "ligh-to-temp" maps
      humidity = convertNumber temperature $ Map.lookup "temp-to-humi" maps
      location = convertNumber humidity $ Map.lookup "humi-to-loca" maps
  in location


-- Find minimal location as minimum os seeds' locations
findLocation :: ([Int], Map.Map String IvMap) -> Int
findLocation input =
  let seeds = fst input
      maps = snd input
  in minimum $ map (fullConversion maps) seeds
-}


-- Find appropriate mapping for a list of seeds
convertNumbers :: [Int] -> Maybe IvMap -> [Int]
convertNumbers _ Nothing = error "Unpredicted situation"
convertNumbers xs (Just mapping) =
  map (convertNumber mapping) xs
  where
    convertNumber [] x = x
    convertNumber (((lower,upper),y):rest) x
      | x >= lower && x <= upper = x + y
      | otherwise = convertNumber rest x

-- Push a list of seeds through a pipe of mappings to find seed's location
-- and select minimum location
findLocation :: ([Int], Map.Map String IvMap) -> Int
findLocation (seeds,maps) =
  let soil = convertNumbers seeds $ Map.lookup "seed-to-soil" maps
      fertilizer = convertNumbers soil $ Map.lookup "soil-to-fert" maps
      water = convertNumbers fertilizer $ Map.lookup "fert-to-wate" maps
      light = convertNumbers water $ Map.lookup "wate-to-ligh" maps
      temperature = convertNumbers light $ Map.lookup "ligh-to-temp" maps
      humidity = convertNumbers temperature $ Map.lookup "temp-to-humi" maps
      location = convertNumbers humidity $ Map.lookup "humi-to-loca" maps
  in minimum location

task1 :: FilePath -> IO Int
task1 filename =
  readFile filename >>= return . parseData >>= return . findLocation

testTask1 :: IO ()
testTask1 = do
  res <- task1 "day5-example"
  case res of
    35 -> putStrLn "OK"
    _  -> putStrLn "something went wrong"

{- Task 2:
To form ranges of numbers (seeds), pipe them through the same set
of mappings as in Task1 and identify the seed corresponding
to the closest location.
-}

-- Prepare ranges of seeds and find minimal location as minimum of
-- minimal locations of seed ranges
findLocation2 :: ([Int], Map.Map String IvMap) -> Int
findLocation2 (seeds,maps) =
  minimum $ map (\(x,y) -> findLocation ([x..y],maps)) $ makeIntervals seeds
  where
    makeIntervals [] = []
    makeIntervals (x:y:rest) = (x,x+y-1) : makeIntervals rest

task2 :: FilePath -> IO Int
task2 filename =
  readFile filename >>= return . parseData >>= return . findLocation2

testTask2 :: IO ()
testTask2 = do
  res <- task2 "day5-example"
  case res of
    46 -> putStrLn "OK"
    _  -> putStrLn "something went wrong"

main :: IO ()
main = do
  x <- task1 "day5-input"
  y <- task2 "day5-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
