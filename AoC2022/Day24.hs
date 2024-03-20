{-# LANGUAGE TupleSections #-}

module Day24 where

import AoCHelper (Pair, between, both, (.+.))
import Data.List (elemIndices, transpose)
import Data.Set (Set, fromList, singleton, (\\))

type Dimension = Pair Int
type Pos = Pair Int

dim' :: Dimension -> Pair Int
dim' = both (pred . pred)

move :: Dimension -> Pos -> [Pos]
move dim pos = let (m', n') = dim' dim in 
  (pos :) . filter (\(x, y) -> x `between` (1, n') && y `between` (1, m'))
  . map (pos .+.) $ [(-1, 0), (1, 0), (0, 1), (0, -1)]
    
moves :: Dimension -> Set Pos -> Set Pos
moves dim = fromList . concatMap (move dim)

mod' :: (Int -> Int) -> Int -> Int -> Int
mod' f m n = succ $ f (n - 1) `mod` m

type BlizzardPositions = ([[Int]], [[Int]], [[Int]], [[Int]])

parseBPs :: [String] -> BlizzardPositions
parseBPs input = (lefts, rights, ups, downs)
  where
    lefts  = f '<' input
    rights = f '>' input
    ups    = g '^' input
    downs  = g 'v' input
    f c = map (elemIndices c)
    g c = f c . transpose

push :: Dimension -> BlizzardPositions -> BlizzardPositions
push dim (lefts, rights, ups, downs) = (lefts', rights', ups', downs')
  where
    lefts'  = f pred n' lefts
    rights' = f succ n' rights
    ups'    = f pred m' ups
    downs'  = f succ m' downs
    f g x = map (map (mod' g x))
    (m', n') = dim' dim

step :: Dimension -> Pos -> (BlizzardPositions, Set Pos) -> (BlizzardPositions, Set Pos)
step (m, n) start (bps, pos) = (bps', pos')
  where
    bps'@(lefts', rights', ups', downs') = push (m, n) bps
    pos' = moves (m, n) pos \\ upsDowns \\ lftsRights
    upsDowns   = f (\x -> map (x, )) n ups' downs'
    lftsRights = f (\y -> map (, y)) m lefts' rights'
    f g d xs ys = fromList . concat . zipWith g [0..d] $ zipWith (++) xs ys

calculate :: Dimension -> Pos -> Pos -> BlizzardPositions -> [(BlizzardPositions, Set Pos)]
calculate dim start end bps =
  takeWhile ((end `notElem`) . snd) . iterate (step dim start) $ (bps, singleton start)

day24 :: [String] -> Pair Int
day24 input = ((,) <$> head <*> sum) . map (succ . length) $ [run, back, again]
  where
    again = calculate' start end $ pushToEnd back
    back = calculate' (n - 2, m - 1) (1, 1) $ pushToEnd run
    run  = calculate' start end $ parseBPs input
    pushToEnd = push dim . push dim . fst . last
    calculate' = calculate dim
    dim = (m, n)
    m = length input
    n = length $ head input
    start = (1, 0)
    end = (n - 2, m - 2)

-- 251
-- 758

main :: IO ()
main = do
  input <- readFile "day24.input"
  print . day24 . lines $ input