module Day22 where

import AoCHelper (Pair, between, splitBy, (.+.))
import Data.Bifunctor (bimap)
import Data.Char (isLetter)

data Motion = L | R deriving (Show, Read)
data Direction = E | S | W | N deriving (Show, Eq, Enum)

type Dimension = Pair Int

rotate :: Direction -> Motion -> Direction
rotate E L = N
rotate x L = pred x
rotate N R = E
rotate x R = succ x

addDir :: Direction -> Pair Int -> Pair Int
addDir d = flip (.+.) (case d of
                         E -> ( 1,  0)
                         S -> ( 0,  1)
                         W -> (-1,  0)
                         N -> ( 0, -1))

step' :: (Direction, Pair Int) -> (Direction, Pair Int)
step' (d, (x, y))
  | y == 199                           && d == S = (S, (x + 100, 0))
  | y ==   0 && x `between` (100, 149) && d == N = (N, (x - 100, 199))
  | y ==  49 && x `between` (100, 149) && d == S = (W, (99, x - 50))
  | x ==  99 && y `between` ( 50,  99) && d == E = (N, (y + 50, 49))
  | x ==  50 && y `between` ( 50,  99) && d == W = (S, (y - 50, 100))
  | y == 100 && x `between` (  0,  49) && d == N = (E, (50, x + 50))

  | x == 149                           && d == E = (W, (99, 149 - y))
  | x ==  99 && y `between` (100, 149) && d == E = (W, (149, 149 - y))
  | x ==   0 && y `between` (100, 149) && d == W = (E, (50, 149 - y))
  | x ==  50 && y `between` (  0,  49) && d == W = (E, ( 0, 149 - y))
  
  | y ==   0 && x `between` ( 50,  99) && d == N = (E, (0, x + 100))
  | x ==   0 && y `between` (150, 199) && d == W = (S, (y - 100, 0))
  | x ==  49 && y `between` (150, 199) && d == E = (N, (y - 100, 149))
  | y == 149 && x `between` ( 50,  99) && d == S = (W, (49, x + 100))
  
  | otherwise = (d, addDir d (x, y))

cubeMove :: [String] -> Int -> (Direction, Pair Int) -> (Direction, Pair Int)
cubeMove board n (d, (x, y))
  | n == 0 = (d, (x, y))
  | otherwise = let (d', (x', y')) = step' (d, (x, y)) in 
                case board !! y' !! x' of
                  '.' -> cubeMove board (n - 1) (d', (x', y'))
                  _ -> (d, (x, y))

step :: Dimension -> Direction -> Pair Int -> Pair Int
step (m, n) d pos = bimap (`mod` n) (`mod` m) $ addDir d pos

move :: [String] -> Dimension -> (Direction, Int) -> Pair Int -> Pair Int
move board dim (d, n) (x, y)
  | n == 0 = (x, y)
  | otherwise = move' $ step dim d (x, y)
  where
    move' (x', y') = case board !! y' !! x' of
      '.' -> move board dim (d, n - 1) (x', y')
      '#' -> (x, y)
      _ -> move' $ step dim d (x', y')

result :: Direction -> Pair Int -> Int
result dir (col, row) = 1000 * (row + 1) + 4 * (col + 1) + fromEnum dir

parseDay22 :: [String] -> ([String], [Motion], [Int])
parseDay22 input = (board, ms, ns)
  where
    [board, [path]] = splitBy (== "") input
    ns = map read $ splitBy isLetter path
    ms = map (read . (: "")) $ filter isLetter path

day22_1, day22_2 :: [String] -> Int
day22_1 input = result (last ds) . foldl (flip (move board' dim)) (0, 0) $ moves
  where
    (board, ms, ns) = parseDay22 input
    ds = scanl rotate E ms
    moves = zip ds $ ((:) <$> succ . head <*> tail) ns

    dim = (h, w)
    h = length board
    w = maximum $ map length board

    board' = map extend board
    extend xs = take w $ xs ++ repeat ' '

day22_2 input = uncurry result . foldl f (N, (start, 0)) $ moves
  where
    (board, ms, ns) = parseDay22 input
    moves = zip (R : ms) ns

    start = succ . length $ takeWhile (== ' ') $ head board

    f (direction, pos) (motion, n) = cubeMove board n (rotate direction motion, pos)

-- 3590
-- 86382

main :: IO ()
main = do
  input <- readFile "day22.input"
  print . day22_1 . lines $ input
  print . day22_2 . lines $ input
