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

points :: [Card] -> [Int]
points cards = map (2 ^) $ filter (>= 0) $ map ((\x -> x - 1) . length . getSharedValues) cards

part1 :: [Card] -> Int
part1 = sum . points


-- part2 :: Int -> [(Int, [[Int]])] -> Int
-- part2 n [] = n
-- part2 n cards = n + part2 value (take value cards ++ drop 1 cards)
--   where
--     value = v cards !! cardNo
--     (cardNo, card) = head cards

-- f n index cards = n * cardPoints + f cardPoints (index + 1) (take cardPoints cards ++ drop 1 cards)
--   where
--     cardPoints = points cards !! index

--     nextBunch = take cardPoints cards ++ drop 1 cards

main = do
  input <- lines <$> readFile "../example_input.txt"
  let cards = parse input

  let ys = map (2 ^) $ filter (>= 0) $ map ((\x -> x - 1) . length . getSharedValues) cards

  putStrLn $ "Part 1: " ++ show (part1 cards)
  -- putStrLn $ "Part 2: " ++ show (f 1 0 cards)
