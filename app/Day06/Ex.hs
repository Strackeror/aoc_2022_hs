module Day06.Ex (part1, part2) where

import qualified Data.Set as Set
import Data.List.Split

allDiff array = Set.size (Set.fromList array) == length array

splitAtNCommon count array =
  let first = head array
      (firstN, rest) = splitAt count array
  in
    if allDiff firstN then
      (firstN, rest)
    else
      let (before, after) = splitAtNCommon count (tail array) in (first:before, after)


part1 :: String -> String
part1 = show . length . fst . splitAtNCommon 4

part2 :: String -> String
part2 = show . length . fst . splitAtNCommon 14
