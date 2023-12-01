module Main where

import Data.Char (isAlpha, isDigit)
import Data.List

spellingToNumber :: String -> Int
spellingToNumber xs
  | "zero"  `isPrefixOf` xs = 0
  | "one"   `isPrefixOf` xs = 1
  | "two"   `isPrefixOf` xs = 2
  | "three" `isPrefixOf` xs = 3
  | "four"  `isPrefixOf` xs = 4
  | "five"  `isPrefixOf` xs = 5
  | "six"   `isPrefixOf` xs = 6
  | "seven" `isPrefixOf` xs = 7
  | "eight" `isPrefixOf` xs = 8
  | "nine"  `isPrefixOf` xs = 9
  | otherwise = 0

-- Reads the first number (or spelling) from a string
convert :: [Char] -> Int
convert [] = 0
convert str
  | isDigit (head str) = read [head str]
  | spellingToNumber str /= 0 = spellingToNumber str
  | otherwise = 0

part1 :: [String] -> Int
part1 [] = 0
part1 input = sum $ map read nums
  where
    nums = map ((\x -> head x : [last x]) . filter isDigit) input

part2 :: [String] -> Int
part2 [] = 0
part2 input = part1 nums
  where
    nums = [show [convert (drop i elem) | (c, i) <- zip elem [0 .. length elem], isDigit c || spellingToNumber (drop i elem) /= 0] | elem <- input]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"

  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
