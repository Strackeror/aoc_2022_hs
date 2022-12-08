module Day08.Ex (part1, part2) where

import Data.List (maximumBy, nub, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Debug.Trace (traceShowId, traceShow)

readInput :: String -> [[Int]]
readInput = map (map read . chunksOf 1) . lines

maxIndex :: [Int] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0 ..]

visibleFilter :: [Int] -> [Bool]
visibleFilter = recurse (-1)
  where
    recurse _ [] = []
    recurse max (x : xs)
      | x > max = True : recurse x xs
      | otherwise = False : recurse max xs

visibleMatrix = map visibleFilter

reverseVisibleMatrix = map (reverse . visibleFilter . reverse)

transposedMatrix = transpose . visibleMatrix . transpose

reverseTransposedMatrix = transpose . reverseVisibleMatrix . transpose

mergeMatrix :: [[Bool]] -> [[Bool]] -> [[Bool]]
mergeMatrix = zipWith $ zipWith (||)

part1 str =
  let matrix = readInput str
   in show $
        sum $
          map (length . filter id) $
              foldl1 mergeMatrix $
                map ($ matrix) [visibleMatrix, reverseVisibleMatrix, transposedMatrix, reverseTransposedMatrix]

part2 _ = ""