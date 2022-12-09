module Main where

import Day01.Ex as Day01 (part1, part2)
import Day02.Ex as Day02 (part1, part2)
import Day03.Ex as Day03 (part1, part2)
import Day04.Ex as Day04 (part1, part2)
import Day05.Ex as Day05 (part1, part2)
import Day06.Ex as Day06 (part1, part2)
import Day07.Ex as Day07 (part1, part2)
import Day08.Ex as Day08 (part1, part2)
import Day09.Ex as Day09 (part1, part2)
import Day10.Ex as Day10 (part1, part2)
import Day11.Ex as Day11 (part1, part2)
import Day12.Ex as Day12 (part1, part2)
import Day13.Ex as Day13 (part1, part2)
import Day14.Ex as Day14 (part1, part2)
import Day15.Ex as Day15 (part1, part2)
import Day16.Ex as Day16 (part1, part2)
import Day17.Ex as Day17 (part1, part2)
import Day18.Ex as Day18 (part1, part2)
import Day19.Ex as Day19 (part1, part2)
import Day20.Ex as Day20 (part1, part2)
import Day21.Ex as Day21 (part1, part2)
import Day22.Ex as Day22 (part1, part2)
import Day23.Ex as Day23 (part1, part2)
import Day24.Ex as Day24 (part1, part2)
import Day25.Ex as Day25 (part1, part2)
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
    (Day08.part1, Day08.part2),
    (Day09.part1, Day09.part2),
    (Day10.part1, Day10.part2),
    (Day11.part1, Day11.part2),
    (Day12.part1, Day12.part2),
    (Day13.part1, Day13.part2),
    (Day14.part1, Day14.part2),
    (Day15.part1, Day15.part2),
    (Day16.part1, Day16.part2),
    (Day17.part1, Day17.part2),
    (Day18.part1, Day18.part2),
    (Day19.part1, Day19.part2),
    (Day20.part1, Day20.part2),
    (Day21.part1, Day21.part2),
    (Day22.part1, Day22.part2),
    (Day23.part1, Day23.part2),
    (Day24.part1, Day24.part2),
    (Day25.part1, Day25.part2)
  ]

main :: IO ()
main = do
  args <- getArgs
  let day = (read $ head args :: Int) - 1
  let file = args !! 1
  content <- readFile file
  putStrLn $ fst (days !! day) content
  putStrLn $ snd (days !! day) content
