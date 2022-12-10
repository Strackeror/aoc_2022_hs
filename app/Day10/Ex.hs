module Day10.Ex (part1, part2) where
import Data.List.Split (splitOn, chunksOf)
import Debug.Trace (traceShowId)
import Data.Bool (bool)

readInstruction :: [Char] -> [Int]
readInstruction =
  choose . splitOn " "
  where
    choose ["noop"] = [0]
    choose ["addx", count] = [0, read count]

strengthAt :: [Int] -> Int -> Int
strengthAt list n = n * list !! (n - 1)

mapWith :: (a -> b -> c) -> a -> [b] -> [c]
mapWith f param = map (f param)

strengthList :: [Int] -> [Int] -> [Int]
strengthList = flip (mapWith strengthAt)

pixelVisible :: Int -> Int -> Bool
pixelVisible index value = abs (value - index) <= 1

toCrt :: [[Bool]] -> String
toCrt = unlines . map (map $ bool ' ' '#')

part1 :: String -> String
part1 =
  show
    . sum
    . strengthList [20, 60, 100, 140, 180, 220]
    . scanl (+) 1
    . concatMap readInstruction
    . lines

part2 :: String -> String
part2 =
   unlines
  . chunksOf 40
  . map (bool '.' '#')
  . zipWith pixelVisible (cycle [0..39])
  . scanl (+) 1
  . concatMap readInstruction
  . lines
