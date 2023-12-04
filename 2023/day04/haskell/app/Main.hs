module Main where

import Data.Char (isSpace)
import Data.List (lines)
import Data.List.Split (splitOn)

type Card = [[Int]]

getSharedValues :: [[Int]] -> [Int]
getSharedValues xs
  | length xs /= 2 = error "Not two lists"
  | otherwise = filter (`elem` head xs) (last xs)

parse :: [String] -> [Card]
parse = map (map (map (read :: String -> Int) . filter (not . null) . splitOn " " . trim) . splitOn "|" . drop 2 . dropWhile (/= ':'))
  where
    trim :: String -> String
    trim = f . f

    f = reverse . dropWhile isSpace

matches :: [Card] -> [Int]
matches = map (length . getSharedValues)

points :: [Card] -> [Int]
points = map ((\x -> if x <= 0 then 0 else 2 ^ x) . (\x -> x - 1)) . matches

part1 :: [Card] -> Int
part1 = sum . points

updateRange :: Int -> Int -> Int -> [Int] -> [Int]
updateRange _ _ _ [] = []
updateRange a b value ls = take a ls ++ u a b value (drop a ls)
  where
    u i p val [] = []
    u i p val (x : xs)
      | i <= p = x + val : u (i + 1) p val xs
      | otherwise = x : u (i + 1) p val xs

counts :: [Card] -> [Int]
counts cards = f 1 (replicate (length cards) 1) cards
  where
    f i countsList cards
      | i >= length cards = countsList
      | otherwise = f (i + 1) (updateRange a b value countsList) cards
      where
        a = i + 1
        b = i + ms !! i
        value = countsList !! i

        ms :: [Int]
        ms = matches cards

part2 :: [Card] -> Int
part2 cards = sum $ counts cards

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"
  let cards = parse input

  let ys = map (2 ^) $ filter (>= 0) $ map ((\x -> x - 1) . length . getSharedValues) cards

  putStrLn $ "Part 2: " ++ show (part1 cards)
  putStrLn $ "Part 2: " ++ show (part2 cards)
