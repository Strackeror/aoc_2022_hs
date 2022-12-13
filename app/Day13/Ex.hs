module Day13.Ex (part1, part2) where
import Data.Either (fromRight)
import Data.List (sortBy)
import Data.Ord (Ordering)
import Text.Parsec (char, choice, digit, many1, newline, optional, parse, sepBy)
import Text.Parsec.String (Parser)

data Tree = Leaf Int | Branch [Tree] deriving (Show, Eq)

num :: Parser Int
num = read <$> many1 digit

tree :: Parser Tree
tree =
  choice
    [ Leaf <$> num
    , Branch <$> (char '[' *> sepBy tree (char ',') <* char ']')
    ]

trees :: Parser [(Tree, Tree)]
trees = many1 $ do
  a <- tree <* newline
  b <- tree <* newline
  optional newline
  return (a, b)

compareTree :: Tree -> Tree -> Ordering
compareTree (Leaf a) (Leaf b) = compare a b
compareTree (Branch []) (Branch []) = EQ
compareTree (Branch []) (Branch (_ : _)) = LT
compareTree (Branch (_ : _)) (Branch []) = GT
compareTree leaf@(Leaf _) branch@(Branch _) = compareTree (Branch [leaf]) branch
compareTree branch@(Branch _) leaf@(Leaf _) = compareTree branch (Branch [leaf])
compareTree (Branch (x : xs)) (Branch (y : ys)) =
  case compareTree x y of
    EQ -> compareTree (Branch xs) (Branch ys)
    GT -> GT
    LT -> LT

part1 =
  show
    . sum
    . map fst
    . filter ((/= GT) . uncurry compareTree . snd)
    . zip [1 ..]
    . either (\err -> error ("parsing err " ++ show err)) id
    . parse trees ""

dividers = [Branch [Branch [Leaf 2]], Branch [Branch [Leaf 6]]]
part2 =
  show
    . product
    . map fst
    . filter (flip elem dividers . snd)
    . zip [1 ..]
    . sortBy compareTree
    . (++ dividers)
    . concatMap (\(a, b) -> [a, b])
    . either (\err -> error ("parsing err " ++ show err)) id
    . parse trees ""
