module Main where

check :: [Int] -> Bool
check xs = sameSign && withinRange
    where
        pairs = zipWith (flip (-)) xs (tail xs)
        sameSign = all (>0) pairs || all (<0) pairs
        withinRange = all (\x -> abs x <= 3 && abs x > 0) pairs

checkRemove :: [Int] -> Bool
checkRemove xs = any (check . (\i -> take i xs ++ drop (i+1) xs)) [0..length xs - 1]

main :: IO ()
main = do
    input <- map words . lines <$> readFile "./input.txt"
    let nums = map (map (\x -> read x :: Int)) input

    let part1 = length $ filter check nums
    let part2 = length $ filter checkRemove nums

    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2