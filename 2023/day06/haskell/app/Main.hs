module Main where
import Data.List.Split (splitOn)

findWinningRange :: Int -> Int -> (Int, Int)
findWinningRange time distance = (floor a, ceiling b)
  where
    a = (d - t) / (-2)
    b = (- d - t) / (-2)

    t :: Float
    t = fromIntegral time
    d = sqrt (fromIntegral (time^2 - 4*distance) :: Float)

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"

  let part1Input = map (map (read :: String -> Int) . filter (/= "") . tail . splitOn " ") input

  let pairs = zip (head part1Input) (last part1Input)
  putStrLn $ "Part 1: " ++ show (map (uncurry findWinningRange) pairs)
  putStrLn $ "Part 1: " ++ show (product $ map ((\(a, b) -> b - a - 1) . uncurry findWinningRange) pairs)

  let part2Input = map ((read :: String -> Int)  . concat . tail . splitOn " ") input
  print $ findWinningRange (head part2Input) (last part2Input)
  putStrLn $ "Part 2: " ++ show ((\(a, b) -> b - a - 1) $ findWinningRange (head part2Input) (last part2Input))
