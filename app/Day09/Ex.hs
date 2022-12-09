module Day09.Ex (part1, part2) where

import Data.List (nub, sort)
import Data.List.Split
import Debug.Trace (traceShowId)

readInstruction :: String -> String
readInstruction str = replicate (read count) char where [[char], count] = splitOn " " str

headUpdate :: (Int, Int) -> Char -> (Int, Int)
headUpdate (x, y) 'U' = (x, y + 1)
headUpdate (x, y) 'D' = (x, y - 1)
headUpdate (x, y) 'L' = (x - 1, y)
headUpdate (x, y) 'R' = (x + 1, y)
headUpdate _ _ = error "invalid direction"

tailDiff :: (Int, Int) -> (Int, Int)
tailDiff (a, b)
  | abs a + abs b >= 3 = (signum a, signum b)
  | abs a + abs b <= 1 = (0, 0)
  | a == 0 = (0, signum b)
  | b == 0 = (signum a, 0)
  | otherwise = (0, 0)

tailUpdate :: (Int, Int) -> (Int, Int) -> (Int, Int)
tailUpdate (tx, ty) (hx, hy) = let (a, b) = tailDiff (hx - tx, hy - ty) in (tx + a, ty + b)

part1 :: String -> String
part1 =
  show
    . length
    . nub
    . scanl tailUpdate (0, 0)
    . scanl headUpdate (0, 0)
    . concatMap readInstruction
    . lines

part2 :: String -> String
part2 =
  show
    . length
    . nub
    . (!! 9)
    . iterate (scanl tailUpdate (0, 0))
    . scanl headUpdate (0, 0)
    . concatMap readInstruction
    . lines
