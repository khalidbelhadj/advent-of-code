module Main where

import Data.Char (ord, isLower, isUpper)
import Data.List

groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n xs
  | length xs < n = error "bruh"
  | otherwise =  take n xs : groupByN n (drop n xs)

value :: Char -> Int
value c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 26 + 1
  | otherwise = error "bruh"

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"
  let halves = map (\x -> splitAt (length x `div` 2) x) input

  let same = map (nub . uncurry intersect) halves
  let part1 = sum $ map (sum . map value) same

  let groups = groupByN 3 input
  let part2 = sum $ map (sum . map value . foldl intersect (['a'..'z'] ++ ['A'..'Z'])) groups

  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2

