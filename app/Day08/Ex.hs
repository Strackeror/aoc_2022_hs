module Day08.Ex (part1, part2) where

import Data.List (elemIndex, findIndex, maximumBy, nub, tails, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Debug.Trace (traceShow, traceShowId)

readInput :: String -> [[Int]]
readInput = map (map read . chunksOf 1) . lines

visibleFromStart :: [Int] -> [Bool]
visibleFromStart = recurse (-1)
 where
  recurse _ [] = []
  recurse max (x : xs)
    | x > max = True : recurse x xs
    | otherwise = False : recurse max xs

countUntil :: (a -> Bool) -> [a] -> Int
countUntil _ [] = 0
countUntil f (x : xs)
  | f x = 1
  | otherwise = 1 + countUntil f xs

visibleAfter :: [Int] -> [Int]
visibleAfter [] = []
visibleAfter (x : xs) = countUntil (>= x) xs : visibleAfter xs

revMap f = map (reverse . f . reverse)

transposeMap f = transpose . map f . transpose

revTransposeMap f = transpose . revMap f . transpose

mapMatrix4Dirs f matrix = [map f matrix, revMap f matrix, transposeMap f matrix, revTransposeMap f matrix]

part1 =
  show
    . sum
    . map (length . filter id)
    . foldl1 (zipWith $ zipWith (||))
    . mapMatrix4Dirs visibleFromStart
    . readInput

part2 =
  show
    . maximum
    . map maximum
    . foldl1 (zipWith $ zipWith (*))
    . mapMatrix4Dirs visibleAfter
    . readInput
