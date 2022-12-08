module Main where

import Day01.Ex as Day01 (part1, part2)
import Day02.Ex as Day02 (part1, part2)
import Day03.Ex as Day03 (part1, part2)
import Day04.Ex as Day04 (part1, part2)
import Day05.Ex as Day05 (part1, part2)
import Day06.Ex as Day06 (part1, part2)
import Day07.Ex as Day07 (part1, part2)
import Day08.Ex as Day08 (part1, part2)
import System.Environment (getArgs)

days :: [(String -> String, String -> String)]
days =
  [ (Day01.part1, Day01.part2),
    (Day02.part1, Day02.part2),
    (Day03.part1, Day03.part2),
    (Day04.part1, Day04.part2),
    (Day05.part1, Day05.part2),
    (Day06.part1, Day06.part2),
    (Day07.part1, Day07.part2),
    (Day08.part1, Day08.part2)
  ]

main :: IO ()
main = do
  args <- getArgs
  let day = (read $ head args :: Int) - 1
  let file = args !! 1
  content <- readFile file
  putStrLn $ fst (days !! day) content
  putStrLn $ snd (days !! day) content
