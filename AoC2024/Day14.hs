module Day14 where

import AoCHelper (Pair, between, (.+.))
import MPCAS (Parser, string, int, char, runParser)
import Data.Bifunctor (bimap)

roboParser :: Parser Maybe (Pair (Pair Int))
roboParser = (,) <$> ((,) <$> (string "p=" *> int) <*> (char ',' *> int)) <* char ' ' <*>
                     ((,) <$> (string "v=" *> int) <*> (char ',' *> int))

parseDay14 :: String -> Pair(Pair Int)
parseDay14 = maybe ((0,0), (0,0)) fst . runParser roboParser

day14 :: [String] -> [[Pair Int]]
day14 input = iterate step ps
  where
    (ps, vs) = unzip $ map parseDay14 input
    step ps' = map (bimap (`mod` 101) (`mod` 103)) $ zipWith (.+.) ps' vs

--                      (101, 103)  
--            ^     --   0 .. 100       50
--            |     --   1 .. 100
--  <- x ->   y     --     ..        51 .. 100    
--            |     -- 101 .. 100
--            v     -- 102 .. 100       50  

qProd :: [Pair Int] -> Int
qProd pic = product $ map (($ pic) . countIn)
              [ ((0, 49), ( 0,  50)), ((51, 100), ( 0,  50))
              , ((0, 49), (52, 102)), ((51, 100), (52, 102))
              ]
  where
    countIn (i1, i2) = length . filter (\(x, y) -> x `between` i1 && y `between` i2)

day14_1, day14_2 :: [String] -> Int
day14_1 = qProd . (!! 100) . day14 
day14_2 = snd . minimum . flip zip [0..] . map qProd . take 10000 . day14
  -- fst . head . dropWhile (not . g . snd) . zip [0..] $ day14 input
  -- length . takeWhile (< 500) . map (size . fromList) $ iterate step ps
  where
    g ps' = any (\(x, y) -> h (x, y) ps') ps'

    h (x, y) ps' = all ((`elem` ps') . ((x, y) .+.)) 
        [                          (0, 0),                      --      x
                          (-1, 1), (0, 1), (1, 1),              --     xxx 
                 (-2, 2), (-1, 2), (0, 2), (1, 2), (2, 2)]      --    xxxxx

-- 214109808
-- 7687

main :: IO ()
main = do
  input <- readFile "day14.input"
  print . day14_1 . lines $ input
  print . day14_2 . lines $ input
