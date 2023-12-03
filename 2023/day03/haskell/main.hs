module Main where
import Data.Char (isAlpha, isDigit)
import Data.List (groupBy)
import Data.Maybe (isJust)
import Data.Bifunctor (second)

data Cell = Cell {
  row :: Int,
  col :: Int,
  value :: Char
} deriving (Eq, Show)

parse :: [String] -> [Cell]
parse input = map (\(col, row, value) -> Cell {row = row, col = col, value = value}) cellTuples
  where
    cellTuples = (concat . zipWith (\i -> zipWith (\j x -> (i, j, x)) [0..]) [0..] ) input

findNumbers :: [Cell] -> [[Cell]]
findNumbers xs = filter (all (isDigit . value)) $ groupBy (\a b -> isDigit (value a) && isDigit (value b)) xs

getCharAt :: [Cell] -> (Int, Int) -> Maybe Char
getCharAt coords (i, j) = f idk
  where
    f :: [Cell] -> Maybe Char
    f [] = Nothing
    f (Cell {value = x}:_) = Just x

    idk = filter (\x -> col x == i && row x == j) coords

getSurroundingCoords :: (Int, Int) -> [(Int, Int)]
getSurroundingCoords (i, j) = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
                               (i, j - 1), (i, j + 1),
                               (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)]

getCloseByStars :: [Cell] -> (Int, Int) -> [(Int, Int)]
getCloseByStars stars (col, row) = [c |c <- getSurroundingCoords (col, row), isJust (getCharAt stars c)]


part1 :: [Cell] -> Int
part1 coords = sum partNumbers
  where
    numbersGrouped = findNumbers coords
    digitCells = filter (isDigit . value) coords

    symbols = filter (\x -> (not . isDigit . value) x && value x /= '.') coords
    digitsCloseToSymbol = [(col, row, n) | Cell {col = col, row = row, value = n} <- digitCells, any isJust [getCharAt symbols c | c <- getSurroundingCoords (col, row)]]

    partNumbers = [read $ map value n | n <- numbersGrouped, or [a == d && b == e | (a, b, c) <- digitsCloseToSymbol, Cell {col = d, row = e, value = f} <- n]]

part2 :: [Cell] -> Int
part2 coords = sum $ map product pairs
  where
    numbersGrouped = findNumbers coords
    digitCells = filter (isDigit . value) coords

    stars = filter ((== '*'). value ) coords
    digitsCloseToStar = map (second head) $ filter (not . null . snd) $ map (\c -> (c, getCloseByStars stars (col c, row c))) digitCells

    xs = map (second head) $ filter (not . null .snd) [((map value n, (col . last $ n, row . last $ n)), [i | (c1, i) <- digitsCloseToStar, c2 <- n, c1 == c2]) | n <- numbersGrouped]

    pairs = [[read num1, read num2] | ((num1, (a, b)), (p, q)) <- xs, ((num2, (c, d)), (r, s)) <- xs, p == r && q == s, a < c || (a == c && b < d)]


main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"

  let coords = parse input

  putStrLn $ "Part 1: " ++ show (part1 coords)
  putStrLn $ "Part 2: " ++ show (part2 coords)

