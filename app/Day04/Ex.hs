module Day04.Ex (part1, part2) where

import Data.List.Split

type Range = ((Int, Int), (Int, Int))

readRangePair :: String -> Range
readRangePair str =
  ((a, b), (c, d))
 where
  [[a, b], [c, d]] = map (map read . splitOn "-") $ splitOn "," str

checkFullOverlap :: Range -> Bool
checkFullOverlap ((a, b), (c, d))
  | a <= c && b >= d = True
  | c <= a && d >= b = True
  | otherwise = False

checkAnyOverlap :: Range -> Bool
checkAnyOverlap ((a, b), (c, d)) = a <= d && c <= b

part1 :: String -> String
part1 str =
  show $
    length $
      filter checkFullOverlap $
        readRangePair
          <$> lines str

part2 :: String -> String
part2 str =
  show $
    length $
      filter checkAnyOverlap $
        readRangePair
          <$> lines str
