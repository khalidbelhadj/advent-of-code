{-# HLINT ignore "Use head" #-}
module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Ix (Ix (inRange))
import Data.List (isInfixOf)
import Data.List.Split (keepDelimsL, splitOn)
import Data.Maybe (fromMaybe, listToMaybe)

data Entry = Entry
  { srcRange :: (Int, Int),
    dstRange :: (Int, Int)
  }
  deriving (Eq, Show)

type Table = [Entry]

-- UTILS

ints :: String -> [Int]
ints input = map (read :: String -> Int) $ splitOn " " input

parseTable :: String -> [String] -> Table
parseTable tableName input = map (toEntry . ints) $ tail $ takeWhile (/= "") $ dropWhile (not . isInfixOf tableName) input
  where
    toEntry :: [Int] -> Entry
    toEntry list
      | length list /= 3 = error "Invalid entry"
      | otherwise = Entry (list !! 1, list !! 1 + list !! 2 - 1) (list !! 0, list !! 0 + list !! 2 - 1)

pair :: [Int] -> [(Int, Int)]
pair [] = []
pair [_] = []
pair (x : y : xs) = (x, y + x) : pair xs

entryLookup :: Table -> Int -> Int
entryLookup table value = fromMaybe value tableValue
  where
    tableValue = listToMaybe [d + (value - fst src) | Entry src (d, _) <- table, inRange src value]

rangesIntersect :: (Int, Int) -> (Int, Int) -> Bool
rangesIntersect (a, b) (c, d) = inRange (a, b) c || inRange (a, b) d || inRange (c, d) a || inRange (c, d) b

-- clamp a, b to be within c, d
clampRange :: (Int, Int) -> (Int, Int) -> (Int, Int)
clampRange (a, b) (c, d) = (max a c, min b d)

clampEntryBySrc :: (Int, Int) -> Entry -> Entry
clampEntryBySrc range entry = Entry (clampRange (srcRange entry) range) (clampRange (a, b) (a + diff1, b + diff2))
  where
    (a, b) = dstRange entry
    diff1 = fst range - fst (srcRange entry)
    diff2 = snd range - snd (srcRange entry)

getMappingsInRange :: [Entry] -> (Int, Int) -> [Entry]
getMappingsInRange table range
  | uncurry (==) range = []
  | otherwise = [clampEntryBySrc range $ Entry (s1, s2) (d1, d2) | Entry (s1, s2) (d1, d2) <- table, rangesIntersect (s1, s2) range]

subtractRanges :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
subtractRanges (a, b) (c, d)
  | rangesIntersect (a, b) (c, d) = filter (uncurry (/=)) [(c, max c (e - 1)), (min d (f + 1), d)]
  | otherwise = [(c, d)]
  where
    (e, f) = clampRange (a, b) (c, d)

-- subtract xs from ys
subtractRangeLists :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
subtractRangeLists [] ranges = ranges
subtractRangeLists x [] = x
subtractRangeLists (x : xs) ys = subtractRangeLists xs (concatMap (subtractRanges x) ys)

getUnmappedRanges :: Table -> [(Int, Int)] -> Table
getUnmappedRanges table ranges = map (\x -> Entry x x) remainingRanges
  where
    remainingRanges = subtractRangeLists mappedRangesSrcs ranges

    mappedRangesSrcs :: [(Int, Int)]
    mappedRangesSrcs = map srcRange $ getMappedRanges table ranges

-- Returns all mapped regions in table from ranges
getMappedRanges :: Table -> [(Int, Int)] -> Table
getMappedRanges table = concatMap (getMappingsInRange table)

-- Returns all destination ranges from ranges using table
getDstRanges :: Table -> [(Int, Int)] -> [(Int, Int)]
getDstRanges table ranges = map dstRange $ getMappedRanges table ranges ++ getUnmappedRanges table ranges

minRanges :: [(Int, Int)] -> Int
minRanges = minimum . concatMap (\(a, b) -> [a, b])

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"

  let seeds = ints $ drop 7 $ head $ takeWhile (/= "") input

  let seedSoil   = parseTable "seed-to-soil" input
  let soilFert   = parseTable "soil-to-fertilizer" input
  let fertWater  = parseTable "fertilizer-to-water" input
  let waterLight = parseTable "water-to-light" input
  let lightTemp  = parseTable "light-to-temperature" input
  let tempHum    = parseTable "temperature-to-humidity" input
  let humLoc     = parseTable "humidity-to-location" input

  let soil       = map (entryLookup seedSoil) seeds
  let fert       = map (entryLookup soilFert) soil
  let water      = map (entryLookup fertWater) fert
  let light      = map (entryLookup waterLight) water
  let temp       = map (entryLookup lightTemp) light
  let hum        = map (entryLookup tempHum) temp
  let loc        = map (entryLookup humLoc) hum

  let soilDst    = getDstRanges seedSoil $ pair seeds
  let fertDst    = getDstRanges soilFert soilDst
  let waterDst   = getDstRanges fertWater fertDst
  let lightDst   = getDstRanges waterLight waterDst
  let tempDst    = getDstRanges lightTemp lightDst
  let humDst     = getDstRanges tempHum tempDst
  let locDst     = getDstRanges humLoc humDst

  putStrLn $ "Part 1: " ++ show (minimum loc)
  putStrLn $ "Part 2: " ++ show (minRanges locDst)
