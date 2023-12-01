module Main where
import Data.List (sort)

group :: [String] -> [[String]]
group [] = []
group xs = takeWhile (/= "") xs : group (drop 1 $ dropWhile (/= "") xs)

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"

  let sums = map (sum . (map read :: [String] -> [Int])) $ group input

  let max = maximum sums
  let top_3 = sum $ take 3 $ reverse $ sort sums

  putStrLn $ "Part 1: " ++ show max
  putStrLn $ "Part 2: " ++ show top_3