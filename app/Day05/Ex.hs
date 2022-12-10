module Day05.Ex (part1, part2) where

import Data.Function
import Data.List (transpose)
import Data.List.Split
import Debug.Trace

readInitialState :: [String] -> [String]
readInitialState lineList =
  lineList
    & map (chunksOf 4)
    & map (map (!! 1))
    & transpose
    & map (filter (/= ' '))

readMove :: String -> (Int, Int, Int)
readMove str =
  let ["move", count, "from", from, "to", to] = splitOn " " str
   in (read count, read from - 1, read to - 1)

readInstructions :: String -> ([[Char]], [(Int, Int, Int)])
readInstructions str =
  let [init, moves] = splitOn [""] $ lines str
   in (readInitialState $ take (length init - 1) init, map readMove moves)

update :: Int -> a -> [a] -> [a]
update id new array = take id array ++ [new] ++ drop (id + 1) array

step :: ([a] -> [a]) -> [[a]] -> (Int, Int, Int) -> [[a]]
step operation state (count, from, to) =
  let (moved, newFrom) = splitAt count (state !! from)
      newTo = operation moved ++ (state !! to)
   in update to newTo $ update from newFrom state

part1 :: String -> String
part1 str =
  let (state, moves) = readInstructions str
   in foldl (step reverse) state moves
        & map head
        & show
part2 :: String -> String
part2 str =
  let (state, moves) = readInstructions str
   in foldl (step id) state moves
        & map head
        & show
