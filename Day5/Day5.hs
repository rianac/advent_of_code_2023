module Main where

import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha,isDigit)

type Interval = (Int,Int)
type Conversion = (Interval,Int)
type IvMap = [Conversion]
type Almanac = Map.Map String IvMap

-- Parse input file

wordP :: ReadP String
wordP = munch1 isAlpha

decimalP :: ReadP Int
decimalP = fmap read (munch1 isDigit)

seedsP :: ReadP [Int]
seedsP = string "seeds: " *> decimalP `sepBy` char ' '

makeConversion :: Int -> Int -> Int -> Conversion
makeConversion d s r = ((s, (s + r - 1)), d - s)

intervalP :: ReadP Conversion
intervalP =
  makeConversion <$> decimalP <* char ' ' <*> decimalP <* char ' ' <*> decimalP

intervalsP :: ReadP IvMap
intervalsP = intervalP `sepBy` char '\n'

makeMap :: String -> String -> IvMap -> (String,IvMap)
makeMap s d is = (take 4 s ++ "-to-" ++ take 4 d,is)

mapP :: ReadP (String,IvMap)
mapP =
  makeMap <$> wordP <* string "-to-" <*> wordP <* string " map:\n" <*> intervalsP

almanacP :: ReadP Almanac
almanacP = Map.fromList <$> mapP `sepBy` string "\n\n"

parseData :: String -> ([Int], Almanac)
parseData =
  fst . head . readP_to_S dataP
  where
    dataP = (,) <$> seedsP <* string "\n\n" <*> almanacP <* char '\n' <* eof

{- Task 1:
To convert a set of numbers (seeds) through a pipe of mappings (seeds->soil,
soil-> fertilizer, ..., humidity->location) and identify the seed corresponding
to the closest location.
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
findLocation :: ([Int], Almanac) -> Int
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
findLocation2 :: ([Int], Almanac) -> Int
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
