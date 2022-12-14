import Data.Char (isDigit)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Maybe

data Tree a = Leaf a | Node [Tree a] deriving (Eq, Show)

treeCompare :: Ord a => Tree a -> Tree a -> Maybe Bool
treeCompare (Node []) (Node []) = Nothing
treeCompare (Node []) _ = Just True
treeCompare _ (Node []) = Just False
treeCompare (Leaf a) (Node xs) = treeCompare (Node [Leaf a]) (Node xs)
treeCompare (Node xs) (Leaf a) = treeCompare (Node xs) (Node [Leaf a])
treeCompare (Leaf a) (Leaf b)
  | a < b = Just True
  | a > b = Just False
  | otherwise = Nothing
treeCompare (Node (x : xs)) (Node (y : ys))
  | isNothing (treeCompare x y) = treeCompare (Node xs) (Node ys)
  | otherwise = treeCompare x y

parseTree :: String -> [Tree Int]
parseTree [] = []
parseTree [x]
  | isDigit x = [Leaf (read [x] :: Int)]
  | otherwise = error "Invalid input"
parseTree (x : xs)
  | isDigit x = Leaf (read p :: Int) : parseTree q
  | x == ',' = parseTree xs
  | x == '[' = Node (parseTree $ removeBrackets a) : parseTree b
  where
    (a, b) = takeToClosingBracket (x : xs)
    (p, q) = span isDigit (x : xs)

removeBrackets :: String -> String
removeBrackets [] = []
removeBrackets xs = tail $ init xs

takeToClosingBracket :: String -> (String, String)
takeToClosingBracket [] = ([], [])
takeToClosingBracket xs = head $ filter (\(a, b) -> tokenSum a == 0) [splitAt i xs | i <- [1 .. (length xs)]]
  where
    tokenSum :: String -> Int
    tokenSum ys = sum $ map token ys
      where
        token :: Char -> Int
        token c
          | c == '[' = 1
          | c == ']' = -1
          | otherwise = 0

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = [(x, x)]
pairs (x : y : xs) = (x, y) : pairs xs

comp :: Ord a => Tree a -> Tree a -> Ordering
comp a b
  | isNothing (treeCompare a b) = EQ
  | fromJust (treeCompare a b) = LT
  | otherwise = GT

main :: IO ()
main = do
  let input = lines <$> readFile "input.txt"
  comparedPairs <- map (uncurry treeCompare) <$> pairs <$> map (head . parseTree) <$> filter (/= "") <$> input
  let numOfPairs = length comparedPairs
  let correctOrderIndices = map fst $ filter (\(a, b) -> b == Just True || isNothing b) $ zip [1 .. numOfPairs] comparedPairs

  putStr "Part 1: "
  print $ sum correctOrderIndices

  let a = Node [Node [Leaf 2]]
  let b = Node [Node [Leaf 6]]
  sortedList <- sortBy comp <$> (++) [a, b] <$> map (head . parseTree) <$> filter (/= "") <$> input
  let indices = [i | (i, elem) <- zip [1 .. (length sortedList)] sortedList, elem == a || elem == b]

  putStr "Part 2: "
  print $ product indices
