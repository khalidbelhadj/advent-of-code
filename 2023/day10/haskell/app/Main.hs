module Main where

import Data.List (find)
import Data.Map (Map, adjust, elems, fromList, keys, lookup, (!))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import qualified Data.ByteString as Prelue

data Cell = Cell
  { position :: (Int, Int),
    value :: Char,
    neighbors :: [(Int, Int)],
    distance :: Int
  }
  deriving (Eq, Show)

type Graph = Map (Int, Int) Cell

parse :: [String] -> Graph
parse input = Data.Map.fromList cellList
  where
    cellList = Prelude.map (\(row, col, val) -> ((row, col), Cell (row, col) val (getNeighbours val (row, col)) 0)) cellTuples

    getNeighbours :: Char -> (Int, Int) -> [(Int, Int)]
    getNeighbours value (row, col) = Prelude.filter (\(a, b) -> a >= 0 && b >= 0) neighbors
      where
        neighbors = case value of
          '-' -> [(row, col - 1), (row, col + 1)]
          '|' -> [(row - 1, col), (row + 1, col)]
          'L' -> [(row - 1, col), (row, col + 1)]
          '7' -> [(row + 1, col), (row, col - 1)]
          'J' -> [(row - 1, col), (row, col - 1)]
          'F' -> [(row + 1, col), (row, col + 1)]
          '.' -> []
          'S' -> [(row + 1, col), (row, col + 1), (row - 1, col), (row, col - 1)]
          _ -> error "Invalid input"

    cellTuples = (concat . zipWith (\i -> zipWith (\j x -> (i, j, x)) [0 ..]) [0 ..]) input

isReverseLinked :: (Int, Int) -> (Int, Int) -> Graph -> Bool
isReverseLinked (r1, c1) (r2, c2) graph = (r1, c1) `elem` neighbors (graph ! (r2, c2))

bfs :: Graph -> (Graph, [(Int, Int)])
bfs graph = case starting of
  Nothing -> error "No starting point found"
  Just x -> (\(a, _, v) -> (a, v)) $ bfs' (graph, [x], [])
  where
    starting :: Maybe (Int, Int)
    starting = position <$> Data.List.find (\x -> value x == 'S') (Data.Map.elems graph)

    bfs' :: (Graph, [(Int, Int)], [(Int, Int)]) -> (Graph, [(Int, Int)], [(Int, Int)])
    bfs' (graph, [], _) = (graph, [], [])
    bfs' (graph, frontier, visited) = bfs' (newGraph, newFrontier, newVisited)
      where
        Cell {distance = d, position = p, neighbors = n} = graph ! (row, col)
        (row, col) = head frontier

        newVisited = (row, col) : visited
        newFrontier = tail frontier ++ neighbors1
        newGraph = Prelude.foldl (flip (Data.Map.adjust (\a -> a {distance = d + 1}))) graph neighbors1

        neighbors1 = Prelude.filter (\x -> isReverseLinked (row, col) x graph && x `notElem` frontier && x `notElem` visited || (x `elem` visited && distance (fromJust $ Data.Map.lookup x graph) > d + 1)) n

part1 :: Graph -> Int
part1 graph = maximum $ Prelude.map distance $ Data.Map.elems newGraph
  where
    (newGraph, _) = bfs graph

part2 :: Graph -> Int
part2 graph = length $ Prelue.filter isEnclosed dots
  where
    (newGraph, cycle) = bfs graph

    dots :: [Cell]
    dots = Prelude.filter (\x -> value x == '.') $ Data.Map.elems newGraph

    isEnclosed :: Cell -> Bool
    isEnclosed cell = undefined

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"
  let graph = parse input

  putStrLn $ "Part 1: " ++ show (part1 graph)
  putStrLn $ "Part 2: " ++ show (part2 graph)
