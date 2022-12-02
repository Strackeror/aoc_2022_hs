module Main where

import Day01.Ex as Day01
import Day02.Ex as Day02
import Debug.Trace
import System.Environment

days =
  [ (Day01.part1, Day01.part2),
    (Day02.part1, Day02.part2)
  ]

main :: IO ()
main = do
  args <- getArgs
  let day = (read $ head args :: Int) - 1
  let file = args !! 1
  content <- readFile file
  putStrLn $ fst (days !! day) content
  putStrLn $ snd (days !! day) content
