module Main where

import Data.List (words, nub, sort, sortOn, sortBy, minimumBy)
import Debug.Trace

data HandType = FiveOfKind | FourOfKind | FullHouse | ThreeOfKind | TwoPair | OnePair | HighCard
  deriving (Show, Eq, Ord)

data Card = A | K | Q  | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two
  deriving (Show, Eq, Ord, Enum, Bounded)

data Hand = Hand
  { handType :: HandType,
    handCards :: [Card],
    bid :: Int
  }
  deriving (Show, Eq, Ord)

handCompare :: Hand -> Hand -> Ordering
handCompare hand1 hand2 =
  case compare (handType hand1) (handType hand2) of
    EQ -> cardsCompare (handCards hand1) (handCards hand2)
    x  -> x

cardsCompare :: [Card] -> [Card] -> Ordering
cardsCompare _ [] = error "invalid hand"
cardsCompare [] _ = error "invalid hand"
cardsCompare (x:xs) (y:ys)
  | length (x:xs) /= length (y:ys) = error "invalid hand"
  | (x:xs) == (y:ys) = EQ
  | otherwise = case compare x y of
                  EQ -> cardsCompare xs ys
                  x  -> x

handCompareJoker :: Hand -> Hand -> Ordering
handCompareJoker hand1 hand2 =
  case compare (handType hand1) (handType hand2) of
    EQ -> cardsCompareJoker (handCards hand1) (handCards hand2)
    x  -> x

cardsCompareJoker :: [Card] -> [Card] -> Ordering
cardsCompareJoker _ [] = error "invalid hand"
cardsCompareJoker [] _ = error "invalid hand"
cardsCompareJoker (x:xs) (y:ys)
  | length (x:xs) /= length (y:ys) = error "invalid hand"
  | x == J && y == J = cardsCompareJoker xs ys
  | x == J = GT
  | y == J = LT
  | (x:xs) == (y:ys) = EQ
  | otherwise = case compare x y of
                  EQ -> cardsCompareJoker xs ys
                  x  -> x

getCardsType :: [Card] -> HandType
getCardsType xs
  | ys == [5] = FiveOfKind
  | ys == [1, 4] = FourOfKind
  | ys == [2, 3] = FullHouse
  | ys == [1, 1, 3] = ThreeOfKind
  | ys == [1, 2, 2] = TwoPair
  | ys == [1, 1, 1, 2] = OnePair
  | ys == [1, 1, 1, 1, 1] = HighCard
  | otherwise = error "invalid hand"
  where
    ys = sort $ map (count xs) (nub xs)
    count xs x = length $ filter (x==) xs


getHandCards :: String -> [Card]
getHandCards = map getCard
  where
    getCard 'A' = A
    getCard 'K' = K
    getCard 'Q' = Q
    getCard 'J' = J
    getCard 'T' = T
    getCard '9' = Nine
    getCard '8' = Eight
    getCard '7' = Seven
    getCard '6' = Six
    getCard '5' = Five
    getCard '4' = Four
    getCard '3' = Three
    getCard '2' = Two
    getCard _ = error "invalid card"

getHandCardsJoker :: String -> [Card]
getHandCardsJoker = bestHandJoker . getHandCards

getHandTypeJoker :: String -> HandType
getHandTypeJoker xs
  | ys == [5] = FiveOfKind
  | ys == [1, 4] = FourOfKind
  | ys == [2, 3] = FullHouse
  | ys == [1, 1, 3] = ThreeOfKind
  | ys == [1, 2, 2] = TwoPair
  | ys == [1, 1, 1, 2] = OnePair
  | ys == [1, 1, 1, 1, 1] = HighCard
  | otherwise = error "invalid hand"
  where
    ys = sort $ map (\x -> length $ filter (x==) bestHand) (nub bestHand)
    bestHand = bestHandJoker $ getHandCards xs

bestHandJoker :: [Card] -> [Card]
bestHandJoker xs
  | J `elem` xs = handCards $ minimumBy handCompare newHands
  | otherwise = xs
  where
    newHands = [(\ cards -> Hand (getCardsType cards) cards 0)
       (bestHandJoker (jokerValue : deleteFirst J xs)) |
       jokerValue <- filter
                       (/= J) [(minBound :: Card) .. (maxBound :: Card)]]

    deleteFirst _ [] = []
    deleteFirst a (b:bc) | a == b    = bc
                        | otherwise = b : deleteFirst a bc

part1 :: [Hand] -> Int
part1 hands = sum $ zipWith (\ rank hand -> rank * bid hand) [1..] sortedHands
  where
    sortedHands = sortBy (flip handCompare) hands

part2 :: [Hand] -> Int
part2 hands = sum $ zipWith (\ rank hand -> rank * bid hand) [1..] sortedHands
  where
    sortedHands = sortBy (flip handCompareJoker) hands

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"
  let hands = map ((\xs -> Hand (getCardsType (getHandCards $ head xs)) (getHandCards (head xs)) (read (last xs) :: Int)) . Data.List.words) input
  let hands2 = map ((\xs -> Hand (getHandTypeJoker (head xs)) (getHandCards (head xs)) (read (last xs) :: Int)) . Data.List.words) input

  print $ cardsCompare [J, K, Q, J, T] [A, K, Q, J, T]

  putStrLn $ "Part 1: " ++ show (part1 hands)
  putStrLn $ "Part 2: " ++ show (part2 hands2)
