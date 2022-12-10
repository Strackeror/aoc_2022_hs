module Day10.Ex (part1, part2) where

import Data.Bool (bool)
import Data.List.Split (chunksOf, splitOn)
import Debug.Trace (traceShowId)

readInstruction :: [Char] -> [Int]
readInstruction =
  choose . splitOn " "
 where
  choose ["noop"] = [0]
  choose ["addx", count] = [0, read count]

strengthAt :: [Int] -> Int -> Int
strengthAt list n = n * list !! (n - 1)

pixelVisible :: Int -> Int -> Bool
pixelVisible index value = abs (value - index) <= 1

part1 :: String -> String
part1 =
  show
    . sum
    . traceShowId
    . flip map [20, 60, 100, 140, 180, 220]
    . strengthAt
    . scanl (+) 1
    . concatMap readInstruction
    . lines

part2 :: String -> String
part2 =
  unlines
    . chunksOf 40
    . map (bool '.' '#')
    . zipWith pixelVisible (cycle [0 .. 39])
    . scanl (+) 1
    . concatMap readInstruction
    . lines
