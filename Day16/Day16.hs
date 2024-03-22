module Main where

import qualified Data.Array as A
import qualified Data.Set as S

type Position = (Int, Int)
type Step = (Position, Position)
type Grid = A.Array (Int, Int) Char

parseData :: String -> Grid
parseData xs = A.listArray ((1, 1),(length rows, length $ head rows)) tiles
  where
    rows = lines xs
    tiles = concat rows

-- Task 1 --

-- Find all next steps for a beam (including spliting the beam as well as
-- finishing the beam after crossing grid border)
nextStep :: Grid -> (Position, Position) -> Step -> [Step]
nextStep grid ((a, b), (c, d)) (prevPos, actPos) =
  map (\x -> (actPos, x)) $ filter inGrid newPositions
  where
    inGrid (x, y) = a <= x && x <= c && b <= y && y <= d
    actTile = grid A.! actPos
    newPositions = nextPositions actTile prevPos actPos

-- Find all possible next positions given current position and move
-- direction (given by previous position)
nextPositions :: Char -> Position -> Position -> [Position]
nextPositions '.' (a, b) (x, y) = [(x + (x - a), y + (y - b))]
nextPositions '|' (a, b) (x, y) =
  if b == y then [(x + (x - a), y)] else [(x - 1, y), (x + 1, y)]
nextPositions '-' (a, b) (x, y) =
  if a == x then [(x, y + (y - b))] else [(x, y - 1), (x, y + 1)]
nextPositions '\\' (a, b) (x, y) =
  if b == y then [(x, y - (a - x))] else [(x - (b - y), y)]
nextPositions '/' (a, b) (x, y) =
  if b == y then [(x, y + (a - x))] else [(x + (b - y), y)]
nextPositions _ _ _ = error "Unpredicted step sequence"

-- Produce all paths of a beam through the grid
beamPath :: Step -> Grid -> Int
beamPath initStep grid = gridCoverage $ go grid [initStep] $ S.singleton initStep
  where
    go _ [] history = history
    go grid heads history = go grid newHeads $ S.union (S.fromList newHeads) history
      where
        bounds = A.bounds grid
        newHeads =
          filter (`S.notMember` history) . concat . map (nextStep grid bounds) $ heads

-- Count all energized tiles
gridCoverage :: S.Set Step -> Int
gridCoverage = S.size . S.map snd

task1 filename = beamPath ((1,0),(1,1)) . parseData <$> readFile filename

-- Task 2 --

-- Investigate all possible starting points for a beam
allBeams :: Grid -> Int
allBeams grid = maximum $ map (flip beamPath grid) allInits
  where
    ((rmin, cmin), (rmax, cmax)) = A.bounds grid
    allInits =
      [ ((rmin-1, c),(rmin, c)) | c <- [cmin..cmax]]
      <> [ ((rmax+1, c),(rmax, c)) | c <- [cmin..cmax]]
      <> [ ((r, cmin-1),(r, cmin)) | r <- [rmin..rmax]]
      <> [ ((r, cmax+1),(r, cmax)) | r <- [rmin..rmax]]

task2 filename = allBeams . parseData <$> readFile filename

-- Useful staff --

testTask1 =
  (\x -> if x == 46 then "OK" else "Something went wrong")
    <$> task1 "day16-example"

testTask2 =
  (\x -> if x == 51 then "OK" else "Something went wrong")
    <$> task2 "day16-example"

main :: IO ()
main = do
  x <- task1 "day16-input"
  y <- task2 "day16-input"
  putStrLn $ "Task 1: " ++ show x
  putStrLn $ "Task 2: " ++ show y
