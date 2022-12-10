import Data.Functor
import Data.List.Split

type CycleCount = Int
type Immediate = Int

type XVal = (Cycle, Int)
type Instruction = (CycleCount, Immediate)
type Bitmap = [Bool]

-- [(instruction, immediate)] -> [(cycle count, immediate)]
readInstruction :: [String] -> Instruction
readInstruction ["addx", y] = (2, read y :: Int)
readInstruction ["noop"] = (1, 0)
readInstruction _ = (0, 0)

-- [(cycle count, immediate)] -> [(cycle number, X)]
execute :: [Instruction] -> [XVal]
execute xs = execute' $ (0, 1) : xs
  where
    execute' [] = []
    execute' [(a, b)] = [(a, b)]
    execute' ((a, b) : (c, d) : xs) = (a, b) : execute' ((a + c, b + d) : xs)

getXAt :: Int -> [XVal] -> Int
getXAt _ [] = 1
getXAt 0 _ = 1
getXAt n [(a, b)]
  | n >= a = b
  | otherwise = 1
getXAt n ((a, b) : (c, d) : xs)
  | n <= a = 1
  | n > a && n <= c = b
  | otherwise = getXAt n ((c, d) : xs)

render :: [XVal] -> String
render = addNewline . map toStr . renderBitmap
  where
    inRange :: Int -> Int -> Bool
    inRange i x = i == x || i == x - 1 || i == x + 1

    renderBitmap :: [XVal] -> Bitmap
    renderBitmap [] = []
    renderBitmap [(_, _)] = []
    renderBitmap ((a, b) : (c, d) : xs) = [inRange (i `mod` 40) b | i <- [a .. (c - 1)]] ++ renderBitmap ((c, d) : xs)

    toStr :: Bool -> Char
    toStr x
      | x = '#'
      | otherwise = '.'

    addNewline :: String -> String
    addNewline xs = h $ chunksOf 40 xs
      where
        h [] = []
        h (x : xs) = x ++ "\n" ++ h xs

main :: IO ()
main = do

  instructions <- readFile "input.txt" <&> lines <&> map (readInstruction . splitOn " ")
  let output = execute instructions

  let cycles = [20, 60, 100, 140, 180, 220]
  let signalStrengths = [(* cycle) $ getXAt cycle output | cycle <- cycles]

  putStr "Part 1: "
  print signalStrengths

  putStr "Part 2: \n"
  putStr $ render output
