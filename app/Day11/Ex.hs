{-# LANGUAGE InstanceSigs #-}
module Day11.Ex (part1, part2) where

import Text.Parsec (many1, newline, oneOf, optional, parse, sepBy1, string, try)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (choice)
import Text.Parsec.String (Parser)

import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (singleton, sort)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)

data Monkey = Monkey
  { items :: [Int]
  , operation :: Int -> Int
  , test :: Int
  , throw :: (Int, Int)
  , count :: Int
  }

instance Show Monkey where
  show :: Monkey -> String
  show Monkey{..} = show ("Monkey", items, test, throw, count)

addItem :: Int -> Monkey -> Monkey
addItem item Monkey{..} = Monkey{items = items ++ [item], ..}
clearItems :: Monkey -> Monkey
clearItems Monkey{..} = Monkey{items = [], count = count + length items, ..}

-- Parsing
parseIntList :: Parser [Int]
parseIntList = map read <$> sepBy1 (many1 digit) (string ", ")

parseOperation :: Parser (Int -> Int)
parseOperation = do
  arg1 <- choice [string "old", many1 digit]
  op <- string " " *> oneOf "*+" <* string " "
  arg2 <- choice [string "old", many1 digit]
  return
    ( \n ->
        let operation '+' = (+)
            operation '*' = (*)
            chooseArg "old" = n
            chooseArg other = read other
         in operation op (chooseArg arg1) (chooseArg arg2)
    )

parseMonkey :: Parser Monkey
parseMonkey =
  Monkey
    <$> ( (string "Monkey " *> many1 digit *> string ":" *> newline)
            *> (string "  Starting items: " *> parseIntList <* newline)
        )
    <*> (string "  Operation: new = " *> parseOperation <* newline)
    <*> (string "  Test: divisible by " *> (read <$> many1 digit) <* newline)
    <*> ( (,)
            <$> (string "    If true: throw to monkey " *> (read <$> many1 digit) <* newline)
            <*> (string "    If false: throw to monkey " *> (read <$> many1 digit) <* optional newline)
            <* optional newline
        )
    <*> return 0

parseMonkeys :: String -> Map Int Monkey
parseMonkeys = Map.fromList . zip [0 ..] . fromRight (error "failed to parse") . parse (many1 parseMonkey) ""

-- Handling
stepAll :: (Int -> Int) -> Map Int Monkey -> Map Int Monkey
stepAll f n = foldl step n . Map.keys $ n
 where
  stepItem :: Monkey -> Int -> (Int, Int)
  stepItem Monkey{..} value =
    (index, nValue)
   where
    nValue = f . operation $ value
    index = if mod nValue test == 0 then fst throw else snd throw

  newItems :: Monkey -> [(Int, Int)]
  newItems monkey@Monkey{..} = map (stepItem monkey) items

  updateMap :: Map Int Monkey -> (Int, Int) -> Map Int Monkey
  updateMap map (id, value) = Map.update (Just . addItem value) id map

  step :: Map Int Monkey -> Int -> Map Int Monkey
  step monkeys index =
    Map.update (Just . clearItems) index
      . foldl updateMap monkeys
      . newItems
      . fromJust
      $ monkeys !? index

part1 :: String -> String
part1 =
  show
    . (product . take 2 . reverse . sort . map count) -- product of the 2 highest counts
    . Map.elems
    . (!! 20)
    . iterate (stepAll (`div` 3))
    . parseMonkeys

part2 :: String -> String
part2 str =
  let monkeys = parseMonkeys str
      divisorProduct = product . map test . Map.elems $ monkeys
   in show
        . (product . take 2 . reverse . sort . map count) -- product of the 2 highest counts
        . Map.elems
        . traceShowId
        . (!! 10000)
        . iterate (stepAll (`mod` divisorProduct))
        $ monkeys
