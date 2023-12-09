module Main where

predict :: [Int] -> Int
predict [] = 0
predict xs
  | all (== 0) xs = 0
  | otherwise = last xs + predict (zipWith (-) (tail xs) xs)

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"
  let nums = map (map read . words) input :: [[Int]]

  putStrLn $ "Part 1: " ++ show (sum $ map predict nums)
  putStrLn $ "Part 2: " ++ show (sum $ map (predict . reverse) nums)
