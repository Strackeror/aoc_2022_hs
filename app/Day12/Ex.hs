module Day12.Ex (part1, part2) where

import Data.Char (isAsciiLower)
import Data.Foldable (minimumBy)
import Data.List (find, nub)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace (traceShow, traceShowId)
import Text.Printf (printf)

data Tile = Start | End | Elevation Char deriving (Show, Eq)

readTiles :: String -> [[Tile]]
readTiles = map (map tile) . lines
 where
  tile 'S' = Start
  tile 'E' = End
  tile c | isAsciiLower c = Elevation c
  tile _ = error "invalid character"

concatPos :: (a, [(b, c)]) -> [((a, b), c)]
concatPos (a, list) = map (\(b, c) -> ((a, b), c)) list

toPosMap :: [[a]] -> Map (Int, Int) a
toPosMap = Map.fromList . concatMap concatPos . zip [0 ..] . map (zip [0 ..])

canGo :: Tile -> Tile -> Bool
canGo Start (Elevation e) = e <= 'b'
canGo (Elevation e) End = e >= 'y'
canGo (Elevation a) (Elevation b) = b <= succ a
canGo _ _ = False

eachDir :: (Int, Int) -> [(Int, Int)]
eachDir (a, b) = [(a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)]

graph :: Map (Int, Int) Tile -> Map (Int, Int) [(Int, Int)]
graph m =
  Map.mapWithKey validPos m
 where
  validPos key tile = filter (maybe False (canGo tile) . (m !?)) . eachDir $ key

findMinPath :: (Ord k) => [k] -> k -> Map k [k] -> Int
findMinPath starts target graph =
  go Map.empty starts 0
 where
  go shortest [] _ = shortest ! target
  go shortest list n = go newShortest newList (n + 1)
   where
    newShortest = foldl (\acc k -> Map.insertWith min k n acc) shortest list
    newList =
      nub
        . filter (maybe True (> n) . (shortest !?))
        . concatMap (graph !)
        $ list

part1 str =
  show . findMinPath [start] end . graph $ posMap
 where
  posMap = toPosMap . readTiles $ str
  start = fst . Map.findMin $ Map.filter (== Start) posMap
  end = fst . Map.findMin $ Map.filter (== End) posMap

part2 str =
  show . findMinPath starts end . graph $ posMap
 where
  posMap = toPosMap . readTiles $ str
  starts = Map.keys $ Map.filter isStart posMap
  end = fst . Map.findMin $ Map.filter (== End) posMap

  isStart Start = True
  isStart (Elevation 'a') = True
  isStart _ = False
