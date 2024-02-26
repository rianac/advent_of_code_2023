module Main where

import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import Data.Char (isAlpha,isDigit)
import Data.List (sort,sortBy)

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

compareConversions :: Conversion -> Conversion -> Ordering
compareConversions (x,_) (y,_) = compare x y

intervalsP :: ReadP IvMap
intervalsP = sortBy compareConversions <$> intervalP `sepBy` char '\n'

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

-- Find appropriate mapping for a list of seed intervals
convertNumbers :: [Interval] -> Maybe IvMap -> [Interval]
convertNumbers _ Nothing = error "Unpredicted situation"
convertNumbers ys (Just mapping) =
  convert ys mapping
  where
    convert [] _ = []
    convert xs [] = xs
    convert x@(y@(a,b):xs) c@(((l,u),d):cx)
      | b < l = y : convert xs c
      | a < l = (a,l-1) : convert ((l,b):xs) c
      | b <=u = (a+d,b+d) : convert xs c
      | a <= u = (a+d,u+d) : convert ((u+1,b):xs) cx
      | otherwise = convert x cx

-- Push a list of seeds through a pipe of mappings to find seed's location
-- and select minimum location
findLocation :: ([Int] -> [Interval]) -> ([Int], Almanac) -> Int
findLocation f (seed_patterns,maps) =
  let seeds = f seed_patterns
      soil = convertNumbers (sort seeds) $ Map.lookup "seed-to-soil" maps
      fertilizer = convertNumbers (sort soil) $ Map.lookup "soil-to-fert" maps
      water = convertNumbers (sort fertilizer) $ Map.lookup "fert-to-wate" maps
      light = convertNumbers (sort water) $ Map.lookup "wate-to-ligh" maps
      temperature = convertNumbers (sort light) $ Map.lookup "ligh-to-temp" maps
      humidity = convertNumbers (sort temperature) $ Map.lookup "temp-to-humi" maps
      location = convertNumbers (sort humidity) $ Map.lookup "humi-to-loca" maps
  in minimum $ map fst location

-- Prepare ranges of seeds
doubleToInt :: [Int] -> [Interval]
doubleToInt  = map (\x -> (x,x))

task1 :: FilePath -> IO Int
task1 filename =
  (findLocation  doubleToInt) . parseData <$> readFile filename

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

-- Prepare ranges of seeds
makeIntervals [] = []
makeIntervals (x:y:rest) = (x,x+y-1) : makeIntervals rest

task2 :: FilePath -> IO Int
task2 filename =
  (findLocation makeIntervals) . parseData <$> readFile filename

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
