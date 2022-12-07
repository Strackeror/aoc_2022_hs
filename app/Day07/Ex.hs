module Day07.Ex (part1, part2) where

import Control.Monad (void)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Text.Parsec (char, digit, many1, newline, oneOf, string, try)
import Text.Parsec.Combinator (choice)
import Text.Parsec.Prim (parse)
import Text.Parsec.String (Parser)
import Debug.Trace (traceShow)

type Map = Map.Map

data LsEntry = File Int String | Dir String deriving (Show)

data Command = Cd String | Ls [LsEntry] deriving (Show)

num :: Parser Int
num = do
  n <- many1 digit
  return $ read n

name :: Parser String
name = do many1 $ oneOf (['a' .. 'z'] ++ "./")

fileEntry :: Parser LsEntry
fileEntry = do
  number <- num
  void $ char ' '
  filename <- name
  void newline
  return $ File number filename

dirEntry :: Parser LsEntry
dirEntry = do
  void $ string "dir "
  dirname <- name
  void newline
  return $ Dir dirname

lsEntry :: Parser LsEntry
lsEntry = choice [try dirEntry, fileEntry]

cdCommand :: Parser Command
cdCommand = do
  void $ string "$ cd "
  cd <- name
  void newline
  return $ Cd cd

lsCommand :: Parser Command
lsCommand = do
  void $ string "$ ls"
  void newline
  Ls <$> many1 lsEntry

commands :: Parser [Command]
commands = many1 $ choice [try cdCommand, lsCommand]

data FileTree = FolderEntry (Map String FileTree) | FileEntry Int deriving (Show)

data FileSystem = FileSystem {cwd :: [String], fileTree :: FileTree} deriving (Show)

baseFs :: FileSystem
baseFs = FileSystem {cwd = [], fileTree = FolderEntry Map.empty}

makeTreeNode :: LsEntry -> (String, FileTree)
makeTreeNode (File size name) = (name, FileEntry size)
makeTreeNode (Dir name) = (name, FolderEntry Map.empty)

addEntries :: FileTree -> [String] -> [LsEntry] -> FileTree
addEntries (FolderEntry subEntries) [] entries = FolderEntry . Map.fromList . map makeTreeNode $ entries
addEntries (FolderEntry subEntries) (current : rest) entries =
  let curMap = fromJust (Map.lookup current subEntries)
      newMap = addEntries curMap rest entries
   in FolderEntry $ Map.insert current newMap subEntries
addEntries _ _ _ = error "invalid path"

runCommand :: FileSystem -> Command -> FileSystem
runCommand FileSystem {fileTree = fTree} (Cd "/") = FileSystem {cwd = [], fileTree = fTree}
runCommand FileSystem {cwd = cwd, fileTree = fTree} (Cd "..") = FileSystem {cwd = tail cwd, fileTree = fTree}
runCommand FileSystem {cwd = cwd, fileTree = fTree} (Cd name) = FileSystem {cwd = name : cwd, fileTree = fTree}
runCommand FileSystem {cwd = cwd, fileTree = fTree} (Ls entries) = FileSystem {cwd = cwd, fileTree = addEntries fTree (reverse cwd) entries}

totalSize :: FileTree -> Int
totalSize (FileEntry size) = size
totalSize (FolderEntry subs) = sum $ map totalSize $ Map.elems subs

sizeSums :: FileTree -> [Int]
sizeSums (FolderEntry subs) = totalSize (FolderEntry subs) : concatMap sizeSums (Map.elems subs)
sizeSums _ = []

part1 str =
  show $
    sum $
      filter (<= 100000) $
        sizeSums $
          fileTree $
            foldl runCommand baseFs $
              fromRight [] $
                parse commands "" str

part2 :: String -> String
part2 str =
  let sums =
        sizeSums $
          fileTree $
            foldl runCommand baseFs $
              fromRight [] $
                parse commands "" str
      used = head sums
      available = 70 * 1000 * 1000 - used
      needToFree = 30 * 1000 * 1000 - available
   in show $ minimum $ filter (>= needToFree) sums
