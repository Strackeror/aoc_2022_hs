module Day14.Ex (part1, part2) where

import Control.Arrow ((&&&))
import Data.List (find, sortBy, tails)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Text.Parsec (char, choice, digit, many1, newline, optional, parse, sepBy1, string)
import Text.Parsec.String (Parser)

num :: Parser Int
num = read <$> many1 digit

instruction :: Parser [(Int, Int)]
instruction = sepBy1 ((,) <$> (num <* char ',') <*> num) (string " -> ")

readLines :: String -> [[(Int, Int)]]
readLines = either (error . ("error" ++) . show) id . parse (sepBy1 instruction newline) ""

data State = State {walls :: Set (Int, Int), sand :: Set (Int, Int), lastGrain :: Maybe (Int, Int)}

newState walls = State{walls = walls, sand = Set.empty, lastGrain = Just (-1, -1)}

range :: (Ord a, Enum a, Num a) => a -> a -> [a]
range a b
  | a <= b = [a .. b]
  | otherwise = [a, a - 1 .. b]

pointsBetween :: [(Int, Int)] -> [(Int, Int)]
pointsBetween ((a, b) : (a', b') : rest) =
  [(a, b) | a <- range a a', b <- range b b']
    ++ pointsBetween ((a', b') : rest)
pointsBetween [x] = [x]

startPoint = (500, 0)

addCoord (a, b) (a', b') = (a + a', b + b')

repr state = unlines [[char (x, y) | x <- [minx .. maxx]] | y <- [miny .. maxy + 2]]
 where
  set' = Set.insert (500, 0) $ Set.union (walls state) (sand state)
  (minx, maxx) = (minimum &&& maximum) . map fst . Set.toList $ set'
  (miny, maxy) = (minimum &&& maximum) . map snd . Set.toList $ set'
  char c
    | Set.member c (walls state) = '#'
    | Set.member c (sand state) = 'O'
    | otherwise = '.'

insertGrain :: Bool -> (Int, Int) -> State -> State
insertGrain floor grain state@State{..} = case dropGrain floor grain state of
  Just g -> state{lastGrain = Just g, sand = Set.insert g sand}
  Nothing -> state{lastGrain = Nothing}

dropGrain :: Bool -> (Int, Int) -> State -> Maybe (Int, Int)
dropGrain floor start State{..}
  | Set.member start sand = Nothing
  | otherwise = go start
 where
  fullSet = Set.union walls sand
  stop = 1 + (maximum . map snd $ Set.toList walls)
  go grain@(a, b)
    | b >= stop = if floor then Just grain else Nothing
    | otherwise = maybe (Just grain) go available
   where
    testFall = [(0, 1), (-1, 1), (1, 1)]
    checkPositions = map (addCoord grain) testFall
    available = find (`Set.notMember` fullSet) checkPositions

part1 =
  show
    . (+) (-1)
    . length
    . takeWhile (isJust . lastGrain)
    . iterate (insertGrain False (500, 0))
    . newState
    . Set.fromList
    . concatMap pointsBetween
    . readLines

part2 =
  show
    . (+) (-1)
    . length
    . takeWhile (isJust . lastGrain)
    . iterate (insertGrain True (500, 0))
    . newState
    . Set.fromList
    . concatMap pointsBetween
    . readLines