import Data.Ix (Ix (inRange))
import Data.List (find, minimumBy, sortOn, tails, transpose)
import Data.Set (Set, empty, insert, member)
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace
import Data.Heap (MinPrioHeap, empty, insert, view)
import Data.Maybe (isNothing)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Data.Heap as Heap
import qualified Data.Set as Set

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  input <- lines <$> readFile "../input.txt"
  return ()
