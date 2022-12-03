module Day03.Ex (part1, part2) where

import Data.List (elemIndex, find, group, minimumBy, sort, sortOn)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShowId)

priority :: Char -> Int
priority c = fromJust $ elemIndex c " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

organizeByCount :: (Ord k) => [[k]] -> [(k, [Int])]
organizeByCount k =
  let keys = map head $ group . sort $ foldl1 (++) k
      countTuple c = (c, map (length . filter (== c)) k)
   in map countTuple keys

isInAll :: (Foldable t, Ord a1, Num a1) => (a2, t a1) -> Bool
isInAll (_, list) = all (> 0) list

splitHalf :: [a] -> [[a]]
splitHalf str = let (a, b) = splitAt (div (length str) 2) str in [a, b]

part1 :: String -> String
part1 str =
  show $
    sum $
      priority . fst . fromJust . find isInAll . organizeByCount . splitHalf
        <$> lines str

part2 :: String -> String
part2 str =
  show $
    sum $
      priority . fst . fromJust . find isInAll . organizeByCount
        <$> (chunksOf 3 . lines $ str)
