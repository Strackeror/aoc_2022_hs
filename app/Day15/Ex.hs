module Day15.Ex (part1, part2) where

import Data.List (nub, sort)
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (traceShow, traceShowId)
import Text.Parsec
import Text.Parsec.String (Parser)

num :: Parser Int
num = do
  sign <- option "" $ string "-"
  num <- many1 digit
  return $ read $ sign ++ num

data Sensor = Sensor (Int, Int) (Int, Int) deriving (Show)
beacon (Sensor _ b) = b

sensor = do
  sx <- string "Sensor at x=" *> num
  sy <- string ", y=" *> num
  bx <- string ": closest beacon is at x=" *> num
  by <- string ", y=" *> num
  return $ Sensor (sx, sy) (bx, by)

sensors = many1 (sensor <* optional newline)

parseSensors = either (\e -> error $ "error: " ++ show e) id . parse sensors ""

getRangeAtY y (Sensor (sx, sy) (bx, by))
  | distance < diff = Nothing
  | otherwise = Just (sx - distance + diff, sx + distance - diff)
 where
  diff = abs (sy - y)
  distance = abs (bx - sx) + abs (by - sy)

bisect (s, e) (s', e')
  | s' >= s && s' <= e && e' >= s && e' <= e = []
  | s > s' && s <= e' && e >= s' && e < e' = [(s', s - 1), (e + 1, e')]
  | s' >= s && s' <= e = [(e + 1, e')]
  | e' >= s && e' <= e = [(s', s - 1)]
  | otherwise = [(s', e')]

allBisect currentList tuple = concatMap (bisect tuple) currentList
addRange list range = sort $ list ++ foldl allBisect [range] list

part1 str =
  let range = 2000000
      sensors = parseSensors str
      isAtY (Sensor _ (_, by)) = by == range
      beaconAtY = length . nub . map beacon $ filter isAtY sensors
   in show
        . flip (-) beaconAtY
        . sum
        . map ((+ 1) . abs . uncurry (-))
        . foldl addRange []
        . mapMaybe (getRangeAtY 2000000)
        $ sensors

invertRanges (start, end) [] = [(start, end)]
invertRanges (start, end) ((a, b) : rest)
  | start < a = (start, a - 1) : invertRanges (b + 1, end) rest
  | b > end = []
  | otherwise = invertRanges (b + 1, end) rest

part2 =
  let maxCoord = 4 * 1000 * 1000
      coordsY y = invertRanges (0, maxCoord) . foldl addRange [] . mapMaybe (getRangeAtY y)
   in show
        . (\(y, [(x, x')]) -> y + x * maxCoord)
        . head
        . filter (not . null . snd)
        . zip [0 ..]
        . zipWith coordsY [0 .. maxCoord]
        . repeat
        . parseSensors
