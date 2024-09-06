module Main where

import Data.Ix (Ix (inRange))
import Data.List (find, minimumBy, sortOn, tails, transpose)
import Data.Map (Map, empty, insert, (!))
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace

data Point = Point
  { row :: Int,
    col :: Int,
    value :: Char
  }
  deriving (Eq, Show, Ord)

type Grid = [Point]

duplicateEmptyRows :: [String] -> [String]
duplicateEmptyRows [] = []
duplicateEmptyRows (x : xs)
  | all (== '.') x = x : x : duplicateEmptyRows xs
  | otherwise = x : duplicateEmptyRows xs

duplicateEmptyCols :: [String] -> [String]
duplicateEmptyCols = transpose . duplicateEmptyRows . transpose

getGrid :: [String] -> [Point]
getGrid = concat . zipWith (\i -> zipWith (Point i) [0 ..]) [0 ..]

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = abs (row p1 - row p2) + abs (col p1 - col p2)

data State = State
  { current :: Point,
    frontier :: [Point],
    visited :: [Point],
    distances :: Map Point Int
  }
  deriving (Eq, Show)

aStar :: (Int, Int) -> Grid -> Point -> Point -> Int
aStar (maxRow, maxCol) grid start end = distances (aStar' $ State start [] [] (insert start 0 empty)) ! end
  where
    aStar' :: State -> State
    aStar' state
      | c == end = state
      | otherwise = aStar' (State newCurrent newFrontier newVisited newDistances)
      where
        State {current = c, frontier = f, visited = v, distances = d} = state

        currentDistance = d ! c

        newVisited = c : v
        newFrontier = f ++ newNeighbours
        newDistances = foldl (\d1 a -> insert a (currentDistance + 1) d1) d newNeighbours
        newCurrent = minimumBy (\x y -> compare (manhattanDistance x end + newDistances ! x) (manhattanDistance y end + newDistances ! y)) newFrontier

        newNeighbours =
          [ fromJust $ find (\x -> row x == row c + a && col x == col c + b) grid
            | a <- [-1, 0, 1],
              b <- [-1, 0, 1],
              inRange (0, maxRow) (row c + a)
                && inRange (0, maxCol) (col c + b)
                && (isNothing (find (\x -> row x == a && col x == b) v) || (isJust (find (\x -> row x == a && col x == b) f) && currentDistance + 1 < d ! fromJust (find (\x -> row x == a && col x == b) f)))
          ]

main :: IO ()
main = do
  input <- lines <$> readFile "../example_input.txt"

  let grid = getGrid $ duplicateEmptyRows $ duplicateEmptyCols input
  let maxRow = maximum $ map row grid
  let maxCol = maximum $ map col grid

  let targets = filter (\x -> value x == '#') grid
  let targetPairs = pairs targets
  let targetPairDistances = map (uncurry (aStar (maxRow, maxCol) grid)) targetPairs

  -- let fstpair = head targetPairs
  print $ uncurry (aStar (maxRow, maxCol) grid) $ head (tail targetPairs)

  -- putStrLn $ "Part 1: " ++ show (sum targetPairDistances)
