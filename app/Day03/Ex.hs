module Day03.Ex (part1, part2) where

import Control.Arrow ((&&&))
import Data.Function ((&))
import Data.List (elemIndex, find, group, minimumBy, sort, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShowId)

data RucksackElem = RucksackElem
  { char :: Char,
    p1Count :: Int,
    p2Count :: Int
  }
  deriving (Show)

priority :: Char -> Int
priority c = case elemIndex c " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" of
  Nothing -> error "invalid char"
  Just index -> index

counts :: String -> [(Char, Int)]
counts = map (head &&& length) . group . sort

makeElems :: [Char] -> [RucksackElem]
makeElems str =
  let (p1, p2) = splitAt (div (length str) 2) str
      list =
        map (\(char, count) -> (char, (count, 0))) (counts p1)
          ++ map (\(char, count) -> (char, (0, count))) (counts p2)

      dict = M.fromListWith (\(a, b) -> \(a2, b2) -> (a + a2, b + b2)) list
   in map (\(key, (count1, count2)) -> RucksackElem key count1 count2) $ M.toList dict

isInBoth :: RucksackElem -> Bool
isInBoth RucksackElem {char = _, p1Count = p1, p2Count = p2} | p1 /= 0 && p2 /= 0 = True
isInBoth _ = False

part1 :: String -> String
part1 str =
  show $
    sum $
      priority . char . fromJust . find isInBoth . makeElems
        <$> lines str


part2 :: String -> String
part2 str = ""