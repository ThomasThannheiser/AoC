module Day24 where 

import AoCHelper (iter)
import MPCAS (Parser, runParser, symbol)
import Data.List (sort, group, groupBy, lookup, intersect, union)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative ((<|>), some))


{-- parsing stuff --}

symbols = ["e", "w", "se", "sw", "ne", "nw"]
neighbourhood = [(2, 0), (-2, 0), (1, -2), (-1, -2), (1, 2), (-1, 2)]

symDict = zip symbols neighbourhood

direction :: Parser Maybe (Int, Int)
direction = do
  s <- foldr1 (<|>) (map symbol symbols)
  return (fromMaybe (0,0) (lookup s symDict))
  
tile :: Parser Maybe (Int, Int)
tile = do
  nrs <- some direction
  let (xs, ys) = unzip nrs
  return (sum xs, sum ys)

parse :: String -> (Int, Int)
parse = maybe (0,0) fst . runParser tile
        
 --}
 
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = map (\(n, m) -> (n + x, m + y)) neighbourhood

blacks :: [String] -> [(Int, Int)]
blacks = map head . filter (odd . length) . group . sort . map parse

step :: [(Int, Int)] -> [(Int, Int)]
step blacks = map head twoNeighbours `union` intersect oneNeighbour blacks
  where oneNeighbour = concat . filter ((== 1) . length) $ nbs
        twoNeighbours = filter ((== 2) . length) nbs 
        nbs = group . sort . concatMap neighbours $ blacks
        
day24_1 = length . blacks
-- takes round about 20 sec for the 100 steps
day24_2 = length . iter step 100 . blacks

-- 388
-- 4002


main = do
  input <- readFile "day24.input"
  print . day24_1 . lines $ input
  print . day24_2 . lines $ input