module Day7 where

import Data.List (sort)

type Name = String
type Data = Integer

data FSItem = File Name Data | Folder Name [FSItem]

sumUp :: FSItem -> Integer
sumUp (File x y) = y
sumUp (Folder x ys) = sum . map sumUp $ ys

dirSums :: FSItem -> [Integer]
dirSums (File x y) = []
dirSums (Folder x ys) = sumUp (Folder x ys) : concatMap dirSums ys


data Cmd
  = CD Direction
  | MkDir String
  | MkFile String Integer
  | Lst
  | None

data Direction
  = Root
  | Up
  | Down String
  
parseCmd :: String -> Cmd
parseCmd s = case words s of
  ["$", "cd", "/"] -> CD Root
  ["$", "cd", ".."] -> CD Up
  ["$", "cd", name] -> CD (Down name)
  ["$", "ls"] -> Lst
  ["dir", name] -> MkDir name
  [size, name] -> MkFile name (read size)
  _ -> None


data FSCrumb = FSCrumb Name [FSItem] [FSItem]
type FSZipper = (FSItem, [FSCrumb])

evalCmd :: Cmd -> FSZipper -> FSZipper
evalCmd (CD x) = case x of
  Root -> const (Folder "/" [], [])
  Up -> goUp 
  Down name -> goDown name 
evalCmd (MkDir name)  = mkItem (Folder name [])
evalCmd (MkFile name size) = mkItem (File name size) 
evalCmd _  = id

mkItem :: FSItem -> FSZipper -> FSZipper
mkItem item (Folder x xs, c) = (Folder x (item : xs), c)
mkItem _ z = z 

goUp :: FSZipper -> FSZipper
goUp (item, []) = (item, [])
goUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ item : rs), bs)

goDown :: String -> FSZipper -> FSZipper
goDown name (Folder folderName items, bs) =
  let (ls, item : rs) = break ((name ==) . itemName) items in 
  (item, FSCrumb folderName ls rs : bs)
goDown _ z = z

itemName :: FSItem -> String
itemName (Folder folderName _) = folderName
itemName (File fileName _) = fileName

day7 :: [String] -> [Integer]
day7 input = dirSums . fst . head . dropWhile (not . null . snd) . iterate goUp 
             . foldl (flip evalCmd) (Folder "" [], []) $ map parseCmd input

day7_1, day7_2 :: [String] -> Integer
day7_1 = sum . filter (< 100000) . day7
day7_2 input = head . dropWhile (< required) . sort $ sums
  where sums = day7 input
        required = head sums - 70000000 + 30000000

-- 1582412
-- 3696336

main :: IO ()
main = do
  input <- readFile "day7.input"
  print . day7_1 . lines $ input
  print . day7_2 . lines $ input