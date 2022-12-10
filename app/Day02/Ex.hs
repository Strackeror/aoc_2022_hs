module Day02.Ex (part1, part2) where

import Data.Function ((&))
import Data.List.Split (splitOn)

data RPS = Rock | Paper | Scissors deriving (Eq)

data Result = Win | Draw | Lose

lose :: RPS -> RPS
lose Rock = Scissors
lose Scissors = Paper
lose Paper = Rock

win :: RPS -> RPS
win Rock = Paper
win Paper = Scissors
win Scissors = Rock

score :: RPS -> Int
score Rock = 1
score Paper = 2
score Scissors = 3

readRps :: String -> RPS
readRps str
  | str `elem` ["A", "X"] = Rock
  | str `elem` ["B", "Y"] = Paper
  | str `elem` ["C", "Z"] = Scissors
  | otherwise = error "invalid RPS"

readResult :: String -> Result
readResult "X" = Lose
readResult "Y" = Draw
readResult "Z" = Win

findResult :: Result -> RPS -> RPS
findResult Win rps = win rps
findResult Draw rps = rps
findResult Lose rps = lose rps

biscore :: RPS -> RPS -> Int
biscore a b
  | a == b = score b + 3
  | lose a == b = score b + 0
  | otherwise = score b + 6

part1 :: String -> String
part1 str =
  lines str
    & map (map readRps . splitOn " ")
    & map (\[a, b] -> (a, b))
    & map (uncurry biscore)
    & sum
    & show

part2 :: String -> String
part2 str =
  lines str
    & map (splitOn " ")
    & map
      ( \[rpsIn, result] ->
          let rps = readRps rpsIn
              response = findResult (readResult result) rps
           in (rps, response)
      )
    & map (uncurry biscore)
    & sum
    & show
