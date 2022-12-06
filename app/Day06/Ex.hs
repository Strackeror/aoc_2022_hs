module Day06.Ex (part1, part2) where

import Data.List (nub)

countFirstNCommon count array 
  | (nub . take count $ array) == take count array = count
  | otherwise = countFirstNCommon count (tail array) + 1

part1 :: String -> String
part1 = show . countFirstNCommon 4

part2 :: String -> String
part2 = show . countFirstNCommon 14
