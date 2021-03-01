module Day12 where

import Data.List (elemIndex)

nextDirection d = directions !! mod (d' + 1) 4
  where Just d' = elemIndex d directions
        directions = "ESWN"

newDirection d n = iterate nextDirection d !! n'
  where n' = mod (div n 90) 4

evalS1 :: (Char, (Int, Int)) -> (Char, Int) -> (Char, (Int, Int))
evalS1 (o, (ew, ns)) (c, v) = case c of
  'R' -> (newDirection o          v, (ew, ns))
  'L' -> (newDirection o (negate v), (ew, ns))
  'E' -> (o, (ew + v, ns))
  'S' -> (o, (ew    , ns - v))
  'W' -> (o, (ew - v, ns))
  'N' -> (o, (ew    , ns + v))
  'F' -> evalS1 (o, (ew, ns)) (o, v)

evalS2 :: (Char, (Int, Int)) -> ((Char, Int), (Char, (Int, Int))) -> (Char, (Int, Int))
evalS2 (x, (ew, ns)) ((c, v), (o, (v1, v2))) = 
  if c == 'F' then let (_, (ew', ns')) = evalS1 (o, (ew, ns)) ('F', v*v1)
                   in evalS1 (nextDirection o, (ew', ns')) ('F', v*v2)
              else (x, (ew, ns))

updateWP :: (Char, (Int , Int)) -> (Char, Int) -> (Char, (Int, Int))
updateWP (o, (v1, v2)) (c, v) = case o of
  'E' -> let (_, (x, y)) = evalS1 (o, ( v1, -v2)) (c, v) in (o, ( x, -y))
  'S' -> let (_, (x, y)) = evalS1 (o, (-v2, -v1)) (c, v) in (o, (-y, -x)) 
  'W' -> let (_, (x, y)) = evalS1 (o, (-v1,  v2)) (c, v) in (o, (-x,  y))
  'N' -> let (_, (x, y)) = evalS1 (o, ( v2,  v1)) (c, v) in (o, ( y,  x))

evalWP :: (Char, (Int , Int)) -> (Char, Int) -> (Char, (Int, Int))
evalWP (o, (v1, v2)) (c, v) = case c of
  'R' -> (newDirection o          v, (v1, v2))
  'L' -> (newDirection o (negate v), (v1, v2))
  'F' -> (o, (v1, v2))
  _   -> updateWP (o, (v1, v2)) (c, v)

toData :: [String] -> [(Char, Int)]
toData = map (\(x :xs) -> (x, read xs))

sumAbs :: (Int, Int) -> Int
sumAbs (a, b) = abs a + abs b

day12 evalS = sumAbs . snd . foldl evalS ('E', (0,0))
day12_1 = day12 evalS1 . toData
day12_2 lst = day12 evalS2 $ zip (toData lst) wps
  where wps = scanl evalWP ('N', (1, 10)) . toData $ lst

-- 757
-- 51249


main = do
  input <- readFile "day12.input"
  print . day12_1 . lines $ input
  print . day12_2 . lines $ input