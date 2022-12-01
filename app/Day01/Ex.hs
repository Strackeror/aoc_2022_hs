module Day01.Ex (part1, part2) where

import Data.List.Split
import Data.List

sums :: String -> [Int]
sums str = map (sum . (map read . splitOn "\n")) (splitOn "\n\n" str)

part1 :: String -> String
part1 = show . maximum . sums

part2 :: String -> String
part2 = show . sum . take 3 . sortBy (flip compare) . sums
