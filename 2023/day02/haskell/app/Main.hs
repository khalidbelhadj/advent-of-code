module Main where

import Data.Char (isSpace)
import Data.List (lines)
import Data.List.Split (splitOn)

redLimit :: Int
redLimit = 12

greenLimit :: Int
greenLimit = 13

blueLimit :: Int
blueLimit = 14

newtype Round = Round (Int, Int, Int)
  deriving (Show)

type Game = [Round]

parseLine :: [Char] -> Game
parseLine line = p $ map (map (parseItem . trim) . splitOn ",") $ splitOn ";" lineNoPrefix
  where
    lineNoPrefix = drop 2 $ dropWhile (/= ':') line

    parseItem :: String -> (Int, String)
    parseItem "" = error "Empty item"
    parseItem item = (\(x : xs) -> (read x :: Int, head xs)) $ splitOn " " item

    trim :: String -> String
    trim = f . f
      where
        f = reverse . dropWhile isSpace

    p :: [[(Int, String)]] -> Game
    p = map (foldl q (Round (0, 0, 0)))

    q :: Round -> (Int, String) -> Round
    q (Round (r, g, b)) (n, m)
      | m == "red" = Round (r + n, g, b)
      | m == "green" = Round (r, g + n, b)
      | m == "blue" = Round (r, g, b + n)
      | otherwise = Round (r, g, b)

verify :: Game -> Bool
verify = all possible
  where
    possible (Round (r, g, b)) = r <= redLimit && g <= greenLimit && b <= blueLimit

power :: Game -> Int
power xs = maximum reds * maximum greens * maximum blues
  where
    reds = filter (/= 0) $ map (\(Round (r, _, _)) -> r) xs
    greens = filter (/= 0) $ map (\(Round (_, g, _)) -> g) xs
    blues = filter (/= 0) $ map (\(Round (_, _, b)) -> b) xs

part1 :: [Game] -> Int
part1 games = sum $ map fst $ filter snd $ zip [1 ..] $ map verify games

part2 :: [Game] -> Int
part2 games = sum $ map power games

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"

  let games = map parseLine input

  putStrLn $ "Part 1: " ++ show (part1 games)
  putStrLn $ "Part 2: " ++ show (part2 games)
