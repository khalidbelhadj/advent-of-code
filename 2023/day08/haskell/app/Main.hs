module Main where

import Data.List (find)
import Debug.Trace (trace)

data Node = Node
  { name :: String,
    left :: String,
    right :: String
  }
  deriving (Show, Eq)

getNode :: String -> [Node] -> Node
getNode node nodes = head $ filter (\x -> name x == node) nodes

parseNode :: String -> Node
parseNode s = Node name left right
  where
    name = take 3 s
    left = take 3 $ drop 7 s
    right = take 3 $ drop 12 s

f :: String -> [Node] -> String -> Int
f instrs nodes starting = g 0 (getNode starting nodes) instrs
  where
    g :: Int -> Node -> String -> Int
    g acc node [] = g acc node instrs
    g acc node (i : is)
      | last (name node) == 'Z' = acc
      | i == 'L' = g (acc + 1) (getNode (left node) nodes) is
      | i == 'R' = g (acc + 1) (getNode (right node) nodes) is
      | otherwise = error "invalid instruction"

part1 :: String -> [Node] -> Int
part1 instrs nodes = f instrs nodes "AAA"

part2 :: String -> [Node] -> Int
part2 instrs nodes = foldl lcm 1 $ map (f instrs nodes . name) (filter ((== 'A') . last . name) nodes)

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"

  let instrs = head input
  let nodes = map parseNode $ drop 2 input

  putStrLn $ "Part 1: " ++ show (part1 instrs nodes)
  putStrLn $ "Part 2: " ++ show (part2 instrs nodes)