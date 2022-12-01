module Main where

import System.Environment
import Debug.Trace
import Day01.Ex as Day01

days = [(Day01.part1, Day01.part2)]

main :: IO ()
main = do
  args <- getArgs
  let day = (read $ head args :: Int) - 1
  let file = args !! 1
  content <- readFile file
  putStrLn $ fst (days !! day) content
  putStrLn $ snd (days !! day) content
 
