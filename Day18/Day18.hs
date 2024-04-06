module Main where

import qualified Data.Array as A
import Data.Tuple.Extra ((&&&))
import Numeric (readHex)

-- Parse input file --

parseData :: String -> [(String, Int)]
parseData = map (transform . words) . lines
  where
    transform [x, y, _] = (x, read y)

parseData2 :: String -> [(String, Int)]
parseData2 = map (transform . words) . lines
  where
    transform [_, _, z] = (direction $ z !! 7, hexNum z)
    direction x
      | x == '0' = "R"
      | x == '1' = "D"
      | x == '2' = "L"
      | x == '3' = "U"
      | otherwise = error "Unknown direction"
    hexNum = fst . head . readHex . take 5 . drop 2

-- Task 1 --

-- Make a path segment as a list of positions in the given direction
makeSegment :: (Int, Int) -> (String, Int) -> [(Int, Int)]
makeSegment (x, y) ("U", steps) = [ (z,y) | z <- [x - steps .. x - 1]]
makeSegment (x, y) ("D", steps) = reverse [ (z,y) | z <- [x + 1 .. x + steps]]
makeSegment (x, y) ("L", steps) = [ (x,z) | z <- [y - steps .. y - 1]]
makeSegment (x, y) ("R", steps) = reverse [ (x,z) | z <- [y + 1 .. y + steps]]

-- Create a path from separate path segments
makePath :: [(String, Int)] -> [(Int, Int)]
makePath = reverse . init . path
  where
    path = foldl (\r e -> makeSegment (head $ r) e  <> r ) [(0,0)]

-- Mark positions of the given path on a grid of positions
makeArray :: [(Int, Int)] -> A.Array (Int, Int) String
makeArray path = grid A.// map (,"x") path
  where
    (xmin, xmax) = minimum &&& maximum $ map fst path
    (ymin, ymax) = minimum &&& maximum $ map snd path
    grid = A.listArray bounds $ replicate (A.rangeSize bounds) ""
    bounds = ((xmin, ymin), (xmax, ymax))

-- Mark edge positions of the grid as outside (of the path) positions
fillEdges :: A.Array (Int, Int) String -> A.Array (Int, Int) String
fillEdges grid = grid A.// map (,".") (filter ((=="") . (grid A.!)) xs)
  where
    ((rmin, cmin), (rmax, cmax)) = A.bounds grid
    xs = ([(, cmin), (, cmax)] <*> [rmin .. rmax]) <>
         ([(rmin, ), (rmax, )] <*> [cmin + 1 .. cmax - 1])

-- Mark inner positions of the grid located outside of the path
fillTiles :: A.Array (Int, Int) String -> A.Array (Int, Int) String
fillTiles grid
  | null empty = grid
  | otherwise = fillTiles $ grid A.// ((,".") <$> empty)
  where
    empty = filter neib . map fst . filter ((=="") . snd) $ A.assocs grid
    neib (x, y) = any ((==".") . (grid A.!)) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- Find outside positions and count the other positions
lagoon :: A.Array (Int, Int) String -> Int
lagoon = length . filter (/=".") . A.elems . fillTiles . fillEdges

task1 filename =
  lagoon . makeArray . makePath . parseData <$> readFile filename

-- Task 2 --

-- Calculate the last position of a straight path segment
nextNode :: (Int, Int) -> (String, Int) -> (Int, Int)
nextNode (x, y) ("U", steps) = (x - steps, y)
nextNode (x, y) ("D", steps) = (x + steps, y)
nextNode (x, y) ("L", steps) = (x, y - steps)
nextNode (x, y) ("R", steps) = (x, y + steps)

-- Create a path as a list of nodes (positions where two segments cross)
makeBorderNodes :: [(String, Int)] -> [(Int, Int)]
makeBorderNodes = reverse . scanl (\r e -> nextNode r e ) (0, 0)

-- Calculate size of a polygon represented by a list of vertexes
shoeLace :: [(Int, Int)] -> Int
shoeLace nodes = (abs . sum) crossProducts `quot` 2
  where
    rs = map fst nodes
    cs = map snd nodes
    rs' = tail rs
    cs' = tail cs
    crossProducts = zipWith (-) (zipWith (*) rs cs') (zipWith (*) rs' cs)

-- Calculate the area of the lagoon based on Shoelace algorithm
lagoonArea :: [(String, Int)] -> Int
lagoonArea path = shoeLaceArea + perimeter `quot` 2 + 1
  where
    vertices = makeBorderNodes path
    shoeLaceArea = shoeLace vertices
    perimeter = sum $ map snd path

task2 filename = lagoonArea . parseData2 <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 62 then "OK" else "Something went wrong")
    <$> task1 "day18-example"

testTask2 =
  (\x -> if x == 952408144115 then "OK" else "Something went wrong")
    <$> task2 "day18-example"

main = do
  x <- task1 "day18-input"
  y <- task2 "day18-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
